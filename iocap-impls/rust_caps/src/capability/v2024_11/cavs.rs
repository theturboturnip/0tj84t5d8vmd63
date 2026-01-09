#[cfg(feature = "std")]
use assert_hex::assert_eq_hex;

use crate::{capability::{v2024_11::{MAX_INITIAL_LENGTH, MIN_INITIAL_LENGTH}, AddrRange, CapDecodeErr, CapEncodeErr}, checked_bits::{const_shift_right, right_pad, BitInt, CheckAdd, CheckLeftShift, CheckSub}};

type Shifter = CheckLeftShift<u128, 65>;
type Cav1Index = BitInt<u16, 15>;
type Cav1SizeDiv = BitInt<u8, 4>;
type Cav2RangeInt = BitInt<u16, 14>;

#[derive(Debug, PartialEq, Eq)]
pub enum BadCaveatReason {
    Cav1IndexOob,
    Cav1StartOob,
    Cav2BadRange,
    Cav2StartOob,
}

impl From<BadCaveatReason> for CapEncodeErr {
    fn from(_value: BadCaveatReason) -> Self {
        Self::InvalidCaveat
    }
}

impl From<BadCaveatReason> for CapDecodeErr {
    fn from(_value: BadCaveatReason) -> Self {
        Self::InvalidCaveat
    }
}

pub trait ToStartLength {
    fn to_start_length(&self) -> (BitInt<u64, 64>, BitInt<u128, 65>);
    fn to_addr_range(&self) -> Result<AddrRange, BadCaveatReason> {
        let (start, length) = self.to_start_length();
        
        assert!(length.max() <= MAX_INITIAL_LENGTH);

        Ok(AddrRange::base_len(start.val(), length.val()))
    }
    // fn check_access(&self, access_addr: BitInt<u64, 64>, access_width: BitInt<u8, 8>) -> bool;
}

#[derive(Debug, Clone)]
pub struct PartialEncodedInitialRange {
    pub elem_width_log2_minus_10: BitInt<u8, 5>,
    pub log_max_count: BitInt<u8, 5>,
    pub elem_count: BitInt<u32, 28>, // From 1..=(26 set bits + implicit set 27th bit) plus1 i.e. 1..=(1 << 27)
    pub base_addr_shr_10: BitInt<u64, 44>, // Up to 44 bits because the bottom 10 bits are always 0
}

impl ToStartLength for PartialEncodedInitialRange {
    fn to_start_length(&self) -> (BitInt<u64, 64>, BitInt<u128, 65>) {
        let start: BitInt<u64, 54> = right_pad(self.base_addr_shr_10);
        let length = Shifter::shl(
            // Apply a constant left shift of 10 to elem_count, because elem_width_log2 is stored (-10)
            right_pad::<u64, 38, u32, 28>(self.elem_count),
            self.elem_width_log2_minus_10
        ); // It should not be possible for length to exceed MAX_LENGTH
        // Length is bounded between (1 << 10) (minimum elem width) and (1 << 64) (maximum 64-bit addrspace)
        #[cfg(feature = "std")]
        assert_eq_hex!(length.bound(), MIN_INITIAL_LENGTH..=MAX_INITIAL_LENGTH);

        (start.expand(), length)
    }
}

#[derive(Debug, Clone)]
struct PostCaveat1Range {
    elem_width_log_2: BitInt<u8, 7>, // 0..=64
    start: BitInt<u64, 64>,
    // Cap2024_11 EDIT
    // If cav1 represents a index=0 pow2 range that exceeds the size of the initial resource,
    // it is considered legal and the end of the range is clamped to the end of the initial resource.
    // This is equivalent to capping the length to the length of the initial resource, because the start of the range == start of initial resource
    clamp_length: Option<BitInt<u128, 65>>,
    // Cap2024_11 EDIT END
}
impl ToStartLength for PostCaveat1Range {
    fn to_start_length(&self) -> (BitInt<u64, 64>, BitInt<u128, 65>) {
        // Cap2024_11 EDIT
        match self.clamp_length {
            None => {
                let length = Shifter::shl(BitInt::<u8, 1>::constant(1), self.elem_width_log_2);
                // Length may have been cut down from previous so is bounded between 1 and (1 << 64)
                #[cfg(feature = "std")]
                assert_eq_hex!(length.bound(), 1..=MAX_INITIAL_LENGTH);
        
                (self.start, length)
            }
            Some(length) => {
                (self.start, length)
            }
        }
        // Cap2024_11 EDIT END
    }
}

#[derive(Debug, Clone)]
struct PostCaveat2Range {
    elem_count: BitInt<u16, 15>, // max = 1 << 14
    elem_width_log_2: BitInt<u8, 6>, // cav2 considers (cav1.elem_width_log_2 - 14) => 0..=45
    start: BitInt<u64, 64>,
}
impl ToStartLength for PostCaveat2Range {
    fn to_start_length(&self) -> (BitInt<u64, 64>, BitInt<u128, 65>) {
        let length = Shifter::shl(self.elem_count, self.elem_width_log_2);
        // Length may have been cut down from previous so is bounded between 1 and (1 << 59)
        // TODO can't use .bound() here because the bound is different for (cav1.elem_width >= 14) and (cav1.elem_width < 14) case
        // Note that length *may be zero* here TODO figure out if that's OK
        assert!(length.max() <= MAX_INITIAL_LENGTH);
        (self.start, length)
    }
}

fn apply_cav1(initial: &PartialEncodedInitialRange, index: Cav1Index, index_size_div: Cav1SizeDiv) -> Result<PostCaveat1Range, BadCaveatReason> {
    let (start, length) = initial.to_start_length();
    // elem_width_log_2_minus_10 may be pushed to a 6-bit number, e.g. in the case where a full 64-bit addrspace initial resource is cut down by size_div == 0, so there's no point storing the 10-offset.
    let old_elem_width_log_2 = CheckAdd::<u8, 6>::add(initial.elem_width_log2_minus_10, BitInt::<u8, 6>::constant(10));
    let old_elem_count = initial.elem_count;

    // Apply the index caveat.
    // This takes the original range of length element_width * element_count and shrinks it to element_width' * 1.
    // Because we're shrinking element_count, element_width' may be greater than element_width to encompass the whole original range.
    //
    // One condition need to be fulfilled from a security perspective
    // Cap2024_11 EDIT
    // (start'..end') may not exceed (start..end).
    // This is so that in certain cases we can represent a cav1 with a pow2 < (old_elem_width_log2 + log_max_count) that encompasses the whole range - even when count != max_count.
    // If you're trying to encompass the whole range, it implies start' == start i.e. index == 0.
    // So: if index = 0 and (end' > end) then the capability is still valid end' is implicitly clamped to end.
    // Otherwise (end' > end) results in an invalid capability.
    // Cap2024_11 EDIT END
    // --------- INPUTS      ---------
    // start = //input// of length 64
    // index = //input// of length 15
    // size_div = //input// of length 4
    // --------- DEFINITIONS ---------
    // end = start + (old_elem_count << old_elem_width_log_2)
    // elem_width_log_2 = old_elem_width_log_2 + log_max_count - size_div
    // start' = start + (index << elem_width_log_2)
    // end' = start + ((index + 1) << elem_width_log2)
    //
    // Because of how start' and end' are defined we can assume (if no overflow occurs ever) that start' < end'
    // => the initial condition turns into two conditions
    // start' >= start
    //      (this is always true if overflow is banned)
    // end' <= end
    // start + ((index + 1) << elem_width_log2) <= start + (old_elem_count << old_elem_width_log_2)
    //      cancel out start
    // ((index + 1) << elem_width_log2) <= (old_elem_count << old_elem_width_log_2)
    //      expand elem_width_log2
    // ((index + 1) << old_elem_width_log_2 + log_max_count - size_div) <= (old_elem_count << old_elem_width_log_2)
    //      cancel old_elem_width_log_2
    // ((index + 1) << log_max_count - size_div) <= (old_elem_count << 0)
    //      we cannot evaluate the LHS directly because log_max_count=5..=26, size_div = 0..=15 - some cases would have a negative shift
    //      => shift everything left by size_div
    // ((index + 1) << log_max_count) <= (old_elem_count << size_div)
    //      This is always safe in a 64-bit shifter:
    //      (index+1) is 16 bits
    //      log_max_count is 5..=27
    //          => the shift will be at most 27+16=43bits long
    //      old_elem_count is 28 bits long
    //      size_div is 4 bits long, 0..=15
    //          => the shift will be at most 28+15=43bits long
    // Cap2024_11 EDIT
    // if we detect end' > end && index == 0 do not return invalid, just send end' = end.
    // otherwise still return invalid.
    // Cap2024_11 EDIT END

    // log_max_count is technically a per-format static value so this shift could be a mux
    let check_lhs: BitInt<u64, 43> = Shifter::shl(CheckAdd::<u16, 16>::add(index, BitInt::<u8, 1>::constant(1)), initial.log_max_count).shrink();
    let check_rhs: BitInt<u64, 43> = Shifter::shl(old_elem_count, index_size_div).shrink();
    
    // check_lhs <= check_rhs iff check_rhs >= check_lhs
    // Cap2024_11 EDIT
    // In the check_lhs > check_rhs (i.e. check_rhs < check_lhs) and index == 0 case save the length of the initial range as a "clamp to" value.
    // See above edits for why this is correct.
    let clamp_length = match CheckSub::<u64, 43>::sub_overflow(check_rhs, check_lhs) {
        Some(_) => {
            // All good, keep going. Don't need to clamp the end
            None
        },
        None => if index.val() == 0 {
            Some(length)
        } else {
            // check_lhs > check_rhs
            return Err(BadCaveatReason::Cav1IndexOob)
        }
    };
    // Cap2024_11 EDIT END

    // old_elem_width_log_2 = 10..=37
    // log_max_count = 5..=27
    // old_elem_width_log_2+log_max_count = 15..=64, 64 requries 7 bits
    // index_size_div = 0..15
    // old_elem_width_log_2+log_max_count-index_size_div = 0..=64
    let elem_width_log_2 = CheckSub::<u8, 7>::sub(CheckAdd::<u8, 7>::add(old_elem_width_log_2, initial.log_max_count), index_size_div);
    assert_eq!(elem_width_log_2.bound(), 0..=64);

    // Find the starting index
    // We can use previously shown properties that this won't overflow 64 bits
    // (the end might hit 1 << 64, but the start must be at least one block below that)
    //
    // max(old_elem_count) = 1 << 27 (28 bits long)
    // max(log_max_count) = 27
    // max(old_elem_width_log2) = 37
    // min(size_div) = 0
    // ((index + 1) << log_max_count) <= (old_elem_count << 0)
    //      therefore 
    // ((index + 1) << log_max_count) <= (1 << 27)
    //      therefore
    // (index << log_max_count) < (1 << 27)
    // 
    // max(index) = 0x7FFF 
    // elem_width_log_2 = old_elem_width_log_2 + log_max_count - size_div
    // max(old_elem_width_log_2 - size_div) = 37
    // 
    // start' = start + (index << (old_elem_width_log_2 + log_max_count - size_div))
    //      therefore we want
    // (index << (old_elem_width_log_2 + log_max_count - size_div)) < (1 << 64)
    // 
    // (index << (old_elem_width_log_2 + log_max_count - size_div)) < ((1 << 27) << max(old_elem_width_log_2 - size_div))
    //      therefore
    // (index << (old_elem_width_log_2 + log_max_count - size_div)) < ((1 << 27) << 37)
    //      therefore 
    // (index << (old_elem_width_log_2 + log_max_count - size_div)) < (1 << 27+37)
    //      therefore
    // (index << (old_elem_width_log_2 + log_max_count - size_div)) < (1 << 64)
    //
    // NOTE that this depends entirely on check_lhs <= check_rhs proven above.
    // Cap2024_11 EDIT
    // We allow check_lhs > check_rhs if index = 0, in which case start_offset is 0 anyway.
    // Cap2024_11 EDIT END
    // e.g. the trivial case index=1 elem_width_log2 = 64 is only possible if elem_count = (1 << 27), old_elem_width_log2 = 37, size_div = 0
    // e.g. check_lhs = 2 << 27, check_rhs = (1 << 27) << 0, check_lhs > check_rhs => pre-rejected
    // In fact, the only allowed index at that point is 0 for check_lhs = check_rhs = 1 << 27.
    let start_offset = CheckLeftShift::<u64, 64>::shl_proven_no_overflow(index, elem_width_log_2);

    // Expand start to 64-bit here because we're adding start_offset which is based on length which may be 2^64.
    // start is from the initial resource and is thus 54-bit
    let start = match CheckAdd::<u64, 64>::add_overflow(start, start_offset) {
        Some(start) => start,
        None => return Err(BadCaveatReason::Cav1StartOob)
    };
    
    Ok(PostCaveat1Range {
        elem_width_log_2, start, clamp_length
    })
}
// TODO PostCaveat2Range should be expressed in a way that doesn't remove a bunch of shifts from this function
fn apply_cav2(cav1: &PostCaveat1Range, range_x: Cav2RangeInt, range_y_minus_one: Cav2RangeInt) -> Result<PostCaveat2Range, BadCaveatReason> {
    // This caveat automatically shifts elem_width down by 2^14, so range_x (14-bit) and range_y (15-bit, max = 1 << 14) can select a subrange from the previous caveat.
    // More formally:
    //
    // width_log2'' = width_log2' - 14
    // (range_x, range_y) << (width_log2' - 14)     does not exit    (0, 1) << width_log2'
    //
    // shift up by 14
    //
    // (range_x[13:0], range_y[14:0]) << width_log2'      does not exit     (0, 1) << width_log2' + 14
    // range_x is an unsigned int therefore >= 0, so the bottom end doesn't matter
    // range_y is >= range_x
    //
    // range_y[14:0] << width_log2' <= 1 << (width_log2' + 14)
    // 
    // shift down by width_log2'
    //
    // range_y[14:0] <= 1 << 14 => true!
    //
    // a slight complication is that we're doing two things at once here - we're expanding all the (minus_5, shr_5) numbers to their full-width counterparts
    // so that we can select sub-kilobyte ranges.

    // Do the (y-1 >= x) check before finding (y-1)+1, because we don't want 0-length ranges which are the case where y=x => (y-1)<x
    let elem_count_minus_one = match CheckSub::<u16, 15>::sub_overflow(range_y_minus_one, range_x) {
        Some(elem_count_minus_one) => elem_count_minus_one,
        // (format!("caveat 2 range_x {range_x:?} is greater than range_y_minus_one {range_y_minus_one:?}"))
        None => return Err(BadCaveatReason::Cav2BadRange),
    };

    // We can actually discard range_y here
    let elem_count = CheckAdd::<u16, 15>::add(elem_count_minus_one, BitInt::<u8, 1>::constant(1));
    #[cfg(feature = "std")]
    assert_eq_hex!(elem_count.bound(), 1..=(1 << 14));

    // Cap2024_11 EDIT
    // It is guaranteed that the range expressed by cav2 will never exceed the pow2 range expressed by cav1.
    // But previous edits established that in limited cases cav1 may represent a pow2 range that *exceeds* the initial resource (when start' == start but end' == end)
    // so cav2 can shrink to the fullest extent.
    // When converting the cav1 output to a range we solve this by clamping end' to end in that limited case,
    // and we need to apply this logic to cav2 also.
    // Cav2 does not need to be valid in any case where the cav2 range exceeds the initial range, so we can directly return Cav2BadRange in that case.
    // Cap2024_11 EDIT END

    // old elem_width_log_2_minus_10 = 0..=54,
    // old elem_width_log_2 = 10..=64
    // elem_width_log_2 = old elem_width_log_2 - 14
    // 
    // elem_width_log_2 = 0..=50
    //
    // range_x = 14 bits => top bit will get shifted up to position 64
    // elem_count is 15 bits through range_y so (elem_count << elem_width_log_2) will hit the 65th bit.
    let (elem_count, elem_width_log_2, start_offset) = match CheckSub::<u8, 7>::sub_overflow(cav1.elem_width_log_2, BitInt::<u8, 7>::constant(14)) {
        Some(e) => {
            let elem_width_log_2 = e.shrink::<u8, 6>(); // old max = 64, new max = 50
            assert_eq!(elem_width_log_2.bound(), 0..=50);

            // Cap2024_11 EDIT
            if let Some(max_length) = cav1.clamp_length {
                let length = CheckLeftShift::<u128, 65>::shl(CheckAdd::<u16, 15>::add(range_y_minus_one, BitInt::<u8, 1>::constant(1)), elem_width_log_2);
                // If max_length < length, cav2 is OOB
                if CheckSub::<u128, 65>::sub_overflow(max_length, length).is_none() {
                    return Err(BadCaveatReason::Cav2BadRange);
                }
            }
            // Cap2024_11 EDIT END

            let start_offset = CheckLeftShift::<u64, 64>::shl(range_x, elem_width_log_2);
            (elem_count, elem_width_log_2, start_offset)
        },
        None => {
            // cav1.elem_width_log_2 is too small to subdivide into 2^14 pieces.
            // e.g. if the range is 256 bytes long we don't have enough room for 16,384 pieces.
            // => We need to eliminate some bits of the range to fix the precision at 1-byte pieces - so if the range is 256, only use 8 bits of range_y and range_x.
            // This is effectively truncation.
            // Truncation usually requires a variable shift-right (i.e. extracting a bit range starting at a variable position), but we don't have a right-shifter - we only have a left-shifter.
            // Fortunately we have an upper limit on the truncation - it can be at most 14 (range_x = 0, range_y = 1 << 14, cav1_elem_width_log_2 = 0 -> new_range_x = 0, new_length = 1)
            // so we can take values range_x[13:0] and elem_count[14:0], treat them as fixed point numbers with 14 fractional bits (i.e. the integer portion is the maximum right-shift)
            // and then shift those fixed-point numbers *left* by (cav2_elem_width_log_2 + 14) (assuming cav2_elem_width_log_2 = -1 for a 1-bit right shift)
            // i.e. shift them left by cav1_elem_width_log_2(!)
            // and finally take the integer portion post-shift, which is a static window and thus cheap in hardware.
            
            // We actually have two quantities to right-shift: range_x and elem_count.
            // We can shift them in the same cycle, because they're small.
            // Consider 
            // (27 bits) 0'13 | range_x[13:0]
            // The window for the shifted_range_x is
            // (27 bits) shift_range_x[12:0] | X'14
            // (max left shift is 13, which takes one bit off)
            // which means shift_range_x[12:0] = 0'13          if left shift amt = 0
            // and         shift_range_x[12:0] = range_x[13:1] if left shift amt = 13 (max)
            //
            // we can attach elem_count on the end
            //
            // (55 bits) |       0'13       | elem_count'15 |        0'13      | range_x'14 |
            // with shift windows
            // (55 bits) |  shift_elem_count'14  |   X'14   | shift_range_x'13 |    X'14    |
            // which means
            // shift_elem_count[13:0] = 0'13 | elem_count[14] if left shift amt = 0
            // shift_elem_count[13:0] = elem_count[14:1]      if left shift amt = 13 (max)
            //
            // if desired extra 0s can pad the boundary between elem_count and range_x, as long as the X'14 between shift_elem_count and shift_range_x is extended by the same amount.
            // the maximum amount of extra padding is 64-55 = 9 bits.

            // depending on latencies/possible overlapping, it may be desirable to truncate range_x and range_y_minus_one *before* subtracting them and adding one to get elem_count.
            // this may also be desirable because it means the bottom bits of range_x and range_y are entirely meaningless, instead of the current situation where they can influence the calculation of elem_count.
            // additionally, this would bring back the length > 1 guarantee - precalculating elem_count means elem_count[14] could be zero and thus shift_elem_count can be zero.
            // anything that generates these capabilities should zero those bottom bits regardless.
            // TODO leaving this point until I check out the hardware impl.

            // actual proof that it doesn't go out:
            // TODO COMPLETE THIS FOR CAV2 WIDTH = 14 not 9
            //
            // we know at this point (range_y'10 - 1)'9 >= range_x'9
            // range_y = 1..=(1 << 9)
            // we have calculated elem_count'10 = range_y'10 - range_x'9
            // we know at this point elem_count'10 + range_x'9 <= 1 << 9
            // we need to ensure that (elem_count >> (9 - cav1_elem_width_log_2)) + (range_x >> (9 - cav1_elem_width_log_2)) <= (1 << cav1_elem_width_log_2)
            // 
            // we can't just shift left by 9 and cancel out part of the right-shift because right-then-left shift zeroes out the elem_count bottom bits
            // assertion: truncation by shifting right and then shifting left always results in a number smaller than or equal to the original
            // e.g. (x >> (9 - cav1_elem_width_log_2)) << (9 - cav1_elem_width_log_2) < x
            // shift the whole thing left by (9 - cav1_elem_width_log_2)
            // 
            // (elem_count >> (9 - cav1_elem_width_log_2) << (9 - cav1_elem_width_log_2)) + (range_x >> (9 - cav1_elem_width_log_2) << (9 - cav1_elem_width_log_2)) <= (1 << cav1_elem_width_log_2) << (9 - cav1_elem_width_log_2)
            // i.e.
            // (elem_count >> (9 - cav1_elem_width_log_2) << (9 - cav1_elem_width_log_2)) + (range_x >> (9 - cav1_elem_width_log_2) << (9 - cav1_elem_width_log_2)) <= (1 << 9)
            // using the assertion above, we know the individual componens of the addition f(elem_count) and f(range_x) are smaller than or equal to their counterparts elem_count and range_x.
            // we know the shift left cannot produce negative numbers
            // therefore we know that f(elem_count) + f(range_x) <= elem_count + range_x <= (1 << 9)
            // therefore it is true that f(elem_count) + f(range_x) <= (1 << 9)
            // and if you shift it all down by any factor the equalities remain the same
            // 
            // THE PROOF WOULD BE EASIER IF YOU TRUNCATED RANGE_Y-1. JUST SAYIN

            // If elem_width_log_2' == 13, we want the overall effect to be shifting range_x and elem_count down by 1, and setting elem_width_log_2'' to zero
            let cav1_elem_width_log_2: BitInt<u8, 4> = BitInt::new_bound(cav1.elem_width_log_2.val(), cav1.elem_width_log_2.min()..=13);

            // Shift up by (positive number = target + 14)
            let start_offset = CheckLeftShift::<u64, 27>::shl(range_x, cav1_elem_width_log_2);
            // Shift it down by a constant factor of 14, so the overall effect is a shift-right of (positive number - 14)
            let start_offset: BitInt<u16, 13> = const_shift_right(start_offset);

            // This shift can be merged with the above shift coz they're by the same value and both fit inside 64-bits
            let elem_count_shl = CheckLeftShift::<u32, 28>::shl(elem_count, cav1_elem_width_log_2);
            // Previously elem_count was 15-bits long. Now we know cav1_elem_width_log2 <= 13 we know it will be shifted down by at least 1, hence it's now 14 bits long.
            let elem_count: BitInt<u16, 14> = const_shift_right(elem_count_shl);

            if let Some(max_length) = cav1.clamp_length {
                let length = CheckAdd::<u16, 15>::add(elem_count, start_offset);
                // If max_length < length, cav2 is OOB
                if CheckSub::<u128, 65>::sub_overflow(max_length, length).is_none() {
                    return Err(BadCaveatReason::Cav2BadRange);
                }
            }

            (elem_count.expand(), BitInt::constant(0), start_offset.expand())
        }
    };

    // High-level assert that the overall length won't be too large
    assert!(Shifter::shl(CheckAdd::<u16, 15>::add(range_y_minus_one, BitInt::<u8, 1>::constant(1)), elem_width_log_2).max() <= cav1.to_start_length().1.max());

    // This will never overflow on shift but the add could overflow if `start` is big enough.
    // This should never happen in practice because if start was large enough, cav1 would have hit the same issue.
    let start = match CheckAdd::<u64, 64>::add_overflow(cav1.start, start_offset) {
        Some(non_overflowing_value) => non_overflowing_value,
        None => return Err(BadCaveatReason::Cav2StartOob),
    };

    Ok(PostCaveat2Range{elem_count, elem_width_log_2, start})
}

#[derive(Debug, Clone)]
pub enum Caveats {
    Zero,
    One {
        index: Cav1Index,
        index_size_div: Cav1SizeDiv,
    },
    Two {
        index: Cav1Index,
        index_size_div: Cav1SizeDiv,

        range_x: Cav2RangeInt,
        range_y_minus_one: Cav2RangeInt,
    }
}
impl Caveats {
    pub fn apply_to(&self, initial: &PartialEncodedInitialRange) -> Result<AddrRange, BadCaveatReason> {
        match self {
            Caveats::Zero => initial.to_addr_range(),
            Caveats::One { index, index_size_div } => {
                let cav1 = apply_cav1(initial, *index, *index_size_div)?;
                cav1.to_addr_range()
            },
            Caveats::Two { index, index_size_div, range_x, range_y_minus_one } => {
                let cav1 = apply_cav1(initial, *index, *index_size_div)?;
                let cav2 = apply_cav2(&cav1, *range_x, *range_y_minus_one)?;
                cav2.to_addr_range()
            },
        }
    }
}

#[cfg(test)]
mod test {
    #[cfg(not(feature = "std"))]
    const _: () = assert!(false, "'std' must be enabled when running tests");

    use assert_hex::assert_eq_hex;

    use crate::{capability::{v2024_11::{cavs::{apply_cav1, apply_cav2, BadCaveatReason, ToStartLength}, AddrRange}, CapDecodeErr}, checked_bits::BitInt};

    use super::{Caveats, PartialEncodedInitialRange};

    impl PartialEncodedInitialRange {
        /// Purely for testing, this creates a PartialEncodedInitialRange with the correct bit-ranges for the different values
        fn new_test(elem_width_log2_minus_10: u8, log_max_count: u8, elem_count: u32, base_addr_shr_10: u64) -> Self {
            Self {
                elem_width_log2_minus_10: BitInt::new_bound(elem_width_log2_minus_10, 0..=27),
                log_max_count: BitInt::new_bound(log_max_count, 5..=27),
                elem_count: BitInt::new_bound(elem_count, 1..=(1 << 27)), // max = (((implicit 1 << 26) | 0x3FF_FFFF (26 bits)) (27bits total)) + 1 (28 bits total, max = top bit set and rest are 0)
                base_addr_shr_10: BitInt::new(base_addr_shr_10),
            }
        }
    }

    #[test]
    fn test_identity_cav1_on_max_size() -> Result<(), CapDecodeErr> {
        // Take a 32KiB addr range
        let initial = PartialEncodedInitialRange::new_test(
            10-10, // elem_width_log2_minus_10
            5, // log_max_count
            1 << 5, // max for lowest elemwidth = (5 bits of encoded count all 1) + 1 + no implicit 1 = 1 << 5 = 32
            0, // base_addr_shr_10
        );

        assert_eq_hex!(initial.to_addr_range()?, (0..(32<<10)).into(), "initial range wasn't correct");

        // This is the biggest possible range, the least-shrinking caveat should return it exactly
        let cav1 = apply_cav1(&initial, BitInt::new(0), BitInt::new(0))?;
        assert_eq_hex!(cav1.to_addr_range()?, (0..(32<<10)).into(), "cav1 incorrectly shrunk the range");

        Ok(())
    }

    #[test]
    fn test_identity_cav1_on_smaller_size() -> Result<(), CapDecodeErr> {
        // Take a 1KiB addr range
        let initial = PartialEncodedInitialRange::new_test(
            10-10, // elem_width_log2_minus_10
            5, // log_max_count
            1, // max = (((implicit 1 << 26) | 0x3FF_FFFF (26 bits)) (27bits total)) + 1 (28 bits total, max = top bit set and rest are 0)
            0, // base_addr_shr_10
        );

        assert_eq_hex!(initial.to_addr_range()?, (0..(1<<10)).into(), "initial range wasn't correct");

        // cav1 with index_size_div 0 will try to make a 2^(10 + log_max_count - 0) = 2^15 range
        // Cap2024_11 EDIT
        // Because oversized cav1 with index=0 is the identity caveat, it is legal but will not shrink the range.
        let cav1 = apply_cav1(&initial, BitInt::new(0), BitInt::new(0))?;
        assert_eq_hex!(cav1.to_addr_range()?, (0..(1<<10)).into(), "cav1 incorrectly shrunk the range");
        // Cap2024_11 EDIT END

        Ok(())
    }

    // Cap2024_11 EDIT
    #[test]
    fn test_non_identity_too_big_cav1_on_smaller_size() -> Result<(), CapDecodeErr> {
        // Take a 3KiB addr range (1.5 * 2^11)
        let initial = PartialEncodedInitialRange::new_test(
            10-10, // elem_width_log2_minus_10
            5, // log_max_count
            3, // max = (((implicit 1 << 26) | 0x3FF_FFFF (26 bits)) (27bits total)) + 1 (28 bits total, max = top bit set and rest are 0)
            0, // base_addr_shr_10
        );
        assert_eq_hex!(initial.to_addr_range()?, (0..(3<<10)).into(), "initial range wasn't correct");

        // Create a cav1 that represents the second (index=1) 2^11 slice (size_div = 15-11 = 4)
        // This is invalid - a too-big cav1 with (index=0) would be treated as the identity caveat,
        // and a 2^11 slice with (index) = 0 would correctly shrink
        // but even though the (index=1) 2^11 slice does partially overlap the initial resource, it is not counted as an identity caveat because it goes outside the bounds.
        // NOTE this could be sad if people want to try and use cav2 to reduce the third third of the range, but that's tooooo bad.
        let cav1 = apply_cav1(&initial, BitInt::new(1), BitInt::new(3));
        assert_eq!(cav1.unwrap_err(), BadCaveatReason::Cav1IndexOob);

        Ok(())
    }
    // Cap2024_11 EDIT END
    
    #[test]
    fn test_big_unconstrained_direct() -> Result<(), CapDecodeErr>{
        // Take the maximum 64-bit addrspace
        let initial = PartialEncodedInitialRange::new_test(
            37 - 10, // elem_width_log2_minus_10
            27, // log_max_count
            1 << 27, // max = (((implicit 1 << 26) | 0x3FF_FFFF (26 bits)) (27bits total)) + 1 (28 bits total, max = top bit set and rest are 0)
            0, // base_addr_shr_10
        );
        assert_eq_hex!(initial.to_addr_range()?, AddrRange::FULL_ADDR_SPACE, "initial range wasn't correct");

        let cav1 = apply_cav1(&initial, BitInt::new(0), BitInt::new(0))?;
        assert_eq_hex!(cav1.to_addr_range()?, AddrRange::FULL_ADDR_SPACE, "cav1 incorrectly shrunk the range");

        // cav2 = 14 bits in 2024_11 = 0x3FFF
        let cav2 = apply_cav2(&cav1, BitInt::new(0), BitInt::new(0x3FFF))?;
        assert_eq_hex!(cav2.to_addr_range()?, AddrRange::FULL_ADDR_SPACE, "cav2 incorrectly shrunk the range");

        // caveats can be enabled but all-zero
        // (technically this is for the purpose of permissions shrinking, and it's only possible for one caveat to shrink permissions i.e. be null, but for our purposes we want the maths to work)
        let cavs = Caveats::Two { index: BitInt::new(0), index_size_div: BitInt::new(0), range_x: BitInt::new(0), range_y_minus_one: BitInt::new(0x3FFF) };
        assert_eq_hex!(cavs.apply_to(&initial)?, AddrRange::FULL_ADDR_SPACE);

        Ok(())
    }

    #[test]
    fn test_cav2_constraining_kilobyte() -> Result<(), CapDecodeErr>{
        // Take a small range
        let initial = PartialEncodedInitialRange::new_test(
            0, // elem_width_log2_minus_10
            5, // log_max_count
            1, // elem_count
            0, // base_addr_shr_10
        );
        // This should represent 1KiB starting at 0
        assert_eq_hex!(initial.to_addr_range()?, (0..(1 << 10)).into(), "initial range wasn't correct");

        let cav1 = apply_cav1(&initial, BitInt::new(0), BitInt::new(5))?; // can't be zero here because size_div is relative to (elem_width_log_2 + log_max_count)
        assert_eq_hex!(cav1.to_addr_range()?, (0..(1 << 10)).into(), "cav1 incorrectly shrunk the range");

        // 2024_11 uses a 14-bit cav2 so we can shrink this beyond the byte granularity, effectively limited at 1 byte (although we can choose to make a 0-byte range too.)
        let cav2 = apply_cav2(&cav1, BitInt::new(0), BitInt::new(0))?;
        assert_eq_hex!(cav2.to_addr_range()?, (0..0).into(), "cav2 incorrectly shrunk the range");

        // When cav2 can shrink beyond the byte, the leftmost bits of range_x and range_y are used to maintain byte granularity
        let cav2 = apply_cav2(&cav1, BitInt::new(0), BitInt::new(1 << 4))?;
        assert_eq_hex!(cav2.to_addr_range()?, (0..1).into(), "cav2 incorrectly shrunk the range");

        // caveats can be enabled but all-zero
        // (technically this is for the purpose of permissions shrinking, and it's only possible for one caveat to shrink permissions i.e. be null, but for our purposes we want the maths to work)
        let cavs = Caveats::Two { index: BitInt::new(0), index_size_div: BitInt::new(5), range_x: BitInt::new(0), range_y_minus_one: BitInt::new(0) };
        assert_eq_hex!(cavs.apply_to(&initial)?, (0..0).into());

        let cavs = Caveats::Two { index: BitInt::new(0), index_size_div: BitInt::new(5), range_x: BitInt::new(0), range_y_minus_one: BitInt::new(1 << 4) };
        assert_eq_hex!(cavs.apply_to(&initial)?, (0..1).into());


        Ok(())
    }

    #[test]
    fn test_cav2_constraining_byte() -> Result<(), CapDecodeErr>{
        // Take a small range
        let initial = PartialEncodedInitialRange::new_test(
            0, // elem_width_log2_minus_10
            5, // log_max_count
            1, // elem_count
            0, // base_addr_shr_10
        );
        // This should represent 1KiB starting at 0
        assert_eq_hex!(initial.to_addr_range()?, (0..(1 << 10)).into(), "initial range wasn't correct");

        // Caveat 1 can cut this down to a single-byte region
        let cav1 = apply_cav1(&initial, BitInt::new(0), BitInt::new(15))?;
        assert_eq_hex!(cav1.to_addr_range()?, (0..1).into(), "cav1 incorrectly shrunk the range");

        // It doesn't make sense to shrink this range further, *but we might want to change the permissions* so a full-range should still be usable.
        // cav2 = 14 bits in 2024_11 = 0x3FFF
        let cav2 = apply_cav2(&cav1, BitInt::new(0), BitInt::new(0x3FFF))?;
        assert_eq_hex!(cav2.to_addr_range()?, (0..1).into(), "cav2 incorrectly shrunk the range");

        let cavs = Caveats::Two { index: BitInt::new(0), index_size_div: BitInt::new(15), range_x: BitInt::new(0), range_y_minus_one: BitInt::new(0x3FFF) };
        assert_eq_hex!(cavs.apply_to(&initial)?, (0..1).into());

        Ok(())
    }

    #[test]
    fn test_cav2_constraining_zero() -> Result<(), CapDecodeErr>{
        // Take a small range
        let initial = PartialEncodedInitialRange::new_test(
            0, // elem_width_log2_minus_10
            5, // log_max_count
            1, // elem_count
            0, // base_addr_shr_10
        );
        // This should represent 1KiB starting at 0
        assert_eq_hex!(initial.to_addr_range()?, (0..(1 << 10)).into(), "initial range wasn't correct");

        // Caveat 1 can cut this down to a single-byte region
        let cav1 = apply_cav1(&initial, BitInt::new(0), BitInt::new(15))?;
        assert_eq_hex!(cav1.to_addr_range()?, (0..1).into(), "cav1 incorrectly shrunk the range");

        // It doesn't make sense to shrink this range further, but right now it's numerically possible
        let cav2 = apply_cav2(&cav1, BitInt::new(0), BitInt::new(0x0))?;
        assert_eq_hex!(cav2.to_addr_range()?, (0..0).into(), "cav2 incorrectly shrunk the range");

        let cavs = Caveats::Two { index: BitInt::new(0), index_size_div: BitInt::new(15), range_x: BitInt::new(0), range_y_minus_one: BitInt::new(0x0) };
        assert_eq_hex!(cavs.apply_to(&initial)?, (0..0).into());

        Ok(())
    }

    // Cap2024_11 EDIT
    #[test]
    fn test_identity_cav1_can_improve_granularity_of_cav2() -> Result<(), CapDecodeErr> {
        // Take a 1KiB addr range
        let initial = PartialEncodedInitialRange::new_test(
            10-10, // elem_width_log2_minus_10
            5, // log_max_count
            1, // max = (((implicit 1 << 26) | 0x3FF_FFFF (26 bits)) (27bits total)) + 1 (28 bits total, max = top bit set and rest are 0)
            0, // base_addr_shr_10
        );

        assert_eq_hex!(initial.to_addr_range()?, (0..(1<<10)).into(), "initial range wasn't correct");

        // cav1 with index_size_div 0 will try to make a 2^(10 + log_max_count - 0) = 2^15 range
        // Because oversized cav1 with index=0 is the identity caveat, it is legal but will not shrink the range.
        let cav1 = apply_cav1(&initial, BitInt::new(0), BitInt::new(0))?;
        assert_eq_hex!(cav1.to_addr_range()?, (0..(1<<10)).into(), "cav1 incorrectly shrunk the range");

        // Using cav2 here will shrink from 2^15, i.e. down to 2^1 granularity.
        let cav2 = apply_cav2(&cav1, BitInt::new(0), BitInt::new(0x0))?;
        assert_eq_hex!(cav2.to_addr_range()?, (0..2).into(), "cav2 incorrectly shrunk the range");

        let cavs = Caveats::Two { index: BitInt::new(0), index_size_div: BitInt::new(0), range_x: BitInt::new(0), range_y_minus_one: BitInt::new(0x0) };
        assert_eq_hex!(cavs.apply_to(&initial)?, (0..2).into());

        // cav1 with index_size_div 5 will try to make a 2^(10 + log_max_count - 5) = 2^10 range
        // This is legal because it doesn't shrink the range, but it does control the granularity of cav2.
        let cav1 = apply_cav1(&initial, BitInt::new(0), BitInt::new(5))?;
        assert_eq_hex!(cav1.to_addr_range()?, (0..(1<<10)).into(), "cav1 incorrectly shrunk the range");

        // Using cav2 here will shrink from 2^10, NOT 2^15, i.e. down to 2^0 granularity.
        let cav2 = apply_cav2(&cav1, BitInt::new(0), BitInt::new(0x0))?;
        assert_eq_hex!(cav2.to_addr_range()?, (0..0).into(), "cav2 incorrectly shrunk the range");

        let cavs = Caveats::Two { index: BitInt::new(0), index_size_div: BitInt::new(5), range_x: BitInt::new(0), range_y_minus_one: BitInt::new(0x0) };
        assert_eq_hex!(cavs.apply_to(&initial)?, (0..0).into());

        Ok(())
    }

    #[test]
    fn test_identity_cav1_blocks_oob_cav2() -> Result<(), CapDecodeErr> {
        // Take a 1KiB addr range
        let initial = PartialEncodedInitialRange::new_test(
            10-10, // elem_width_log2_minus_10
            5, // log_max_count
            1, // max = (((implicit 1 << 26) | 0x3FF_FFFF (26 bits)) (27bits total)) + 1 (28 bits total, max = top bit set and rest are 0)
            0, // base_addr_shr_10
        );

        assert_eq_hex!(initial.to_addr_range()?, (0..(1<<10)).into(), "initial range wasn't correct");

        // cav1 with index_size_div 0 will try to make a 2^(10 + log_max_count - 0) = 2^15 range
        // Because oversized cav1 with index=0 is the identity caveat, it is legal but will not shrink the range.
        let cav1 = apply_cav1(&initial, BitInt::new(0), BitInt::new(0))?;
        assert_eq_hex!(cav1.to_addr_range()?, (0..(1<<10)).into(), "cav1 incorrectly shrunk the range");

        // Using cav2 here will shrink from 2^15, i.e. down to 2^1 granularity.
        // Because the initial range is 2^10 long, any range_x above 2^10/2^1 = 2^9 will be inside the too-big cav1 bounds but outside the initial range.
        // These should be rejected.
        let cav2 = apply_cav2(&cav1, BitInt::new(1 << 9), BitInt::new((1 << 9) + 1));
        assert_eq!(cav2.unwrap_err(), BadCaveatReason::Cav2BadRange);

        let cavs = Caveats::Two { index: BitInt::new(0), index_size_div: BitInt::new(0), range_x: BitInt::new(1 << 9), range_y_minus_one: BitInt::new((1 << 9) + 1) };
        assert_eq_hex!(cavs.apply_to(&initial), Err(BadCaveatReason::Cav2BadRange));

        Ok(())
    }
    // Cap2024_11 EDIT END

    // TODO test nonzero starting addresses

}