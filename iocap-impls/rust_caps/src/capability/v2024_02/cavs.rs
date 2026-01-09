#[cfg(feature = "std")]
use assert_hex::assert_eq_hex;

use crate::{capability::{AddrRange, CapDecodeErr, CapEncodeErr}, checked_bits::{const_shift_right, right_pad, BitInt, CheckAdd, CheckLeftShift, CheckSub}};

type Adder64 = CheckAdd<u64, 64>;
type Shifter = CheckLeftShift<u128, 65>;
type Cav1Index = BitInt<u16, 15>;
type Cav1SizeDiv = BitInt<u8, 4>;
type Cav2RangeInt = BitInt<u16, 9>;

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
        
        assert!(length.max() <= (1 << 64));

        Ok(AddrRange::base_len(start.val(), length.val()))
    }
    // fn check_access(&self, access_addr: BitInt<u64, 64>, access_width: BitInt<u8, 8>) -> bool;
}

#[derive(Debug, Clone)]
pub struct PartialEncodedInitialRange {
    pub elem_width_log2_minus_10: BitInt<u8, 5>,
    pub log_max_count: BitInt<u8, 5>,
    pub elem_count: BitInt<u32, 28>, // From 1..=(26 set bits + implicit set 27th bit) plus1 i.e. 1..=(1 << 27)
    pub base_addr_shr_10: BitInt<u64, 54>, // Up to 54 bits because the bottom 10 bits are always 0
}

impl PartialEncodedInitialRange {
    const MAX_LENGTH: u128 = 1 << 64;
}
impl ToStartLength for PartialEncodedInitialRange {
    fn to_start_length(&self) -> (BitInt<u64, 64>, BitInt<u128, 65>) {
        let start: BitInt<u64, 64> = right_pad(self.base_addr_shr_10);
        let length = Shifter::shl(
            // Apply a constant left shift of 10 to elem_count, because elem_width_log2 is stored (-10)
            right_pad::<u64, 38, u32, 28>(self.elem_count),
            self.elem_width_log2_minus_10
        ); // It should not be possible for length to exceed MAX_LENGTH
        // Length is bounded between (1 << 10) (minimum elem width) and (1 << 64) (maximum 64-bit addrspace)
        #[cfg(feature = "std")]
        assert_eq_hex!(length.bound(), (1 << 10)..=PartialEncodedInitialRange::MAX_LENGTH);

        (start, length)
    }
}

#[derive(Debug, Clone)]
struct PostCaveat1Range {
    elem_width_log_2: BitInt<u8, 7>, // 0..=64
    start: BitInt<u64, 64>,
}
impl ToStartLength for PostCaveat1Range {
    fn to_start_length(&self) -> (BitInt<u64, 64>, BitInt<u128, 65>) {
        let length = Shifter::shl(BitInt::<u8, 1>::constant(1), self.elem_width_log_2);
        // Length may have been cut down from previous so is bounded between 1 and (1 << 64)
        #[cfg(feature = "std")]
        assert_eq_hex!(length.bound(), 1..=PartialEncodedInitialRange::MAX_LENGTH);

        (self.start, length)
    }
}

#[derive(Debug, Clone)]
struct PostCaveat2Range {
    elem_count: BitInt<u16, 10>, // max = 1 << 9
    elem_width_log_2: BitInt<u8, 6>, // cav2 considers (cav1.elem_width_log_2 - 9) => 6..=55
    start: BitInt<u64, 64>,
}
impl ToStartLength for PostCaveat2Range {
    fn to_start_length(&self) -> (BitInt<u64, 64>, BitInt<u128, 65>) {
        let length = Shifter::shl(self.elem_count, self.elem_width_log_2);
        // Length may have been cut down from previous so is bounded between 1 and (1 << 64)
        // TODO can't use .bound() here because the bound is different for (cav1.elem_width >= 9) and (cav1.elem_width < 9) case
        // Note that length *may be zero* here TODO figure out if that's OK
        assert!(length.max() <= PartialEncodedInitialRange::MAX_LENGTH);
        (self.start, length)
    }
}

fn apply_cav1(initial: &PartialEncodedInitialRange, index: Cav1Index, index_size_div: Cav1SizeDiv) -> Result<PostCaveat1Range, BadCaveatReason> {
    let (start, _) = initial.to_start_length();
    // elem_width_log_2_minus_10 may be pushed to a 6-bit number, e.g. in the case where a full 64-bit addrspace initial resource is cut down by size_div == 0, so there's no point storing the 10-offset.
    let old_elem_width_log_2 = CheckAdd::<u8, 6>::add(initial.elem_width_log2_minus_10, BitInt::<u8, 6>::constant(10));
    let old_elem_count = initial.elem_count;

    // Apply the index caveat.
    // This takes the original range of length element_width * element_count and shrinks it to element_width' * 1.
    // Because we're shrinking element_count, element_width' may be greater than element_width to encompass the whole original range.
    //
    // One condition need to be fulfilled from a security perspective
    // (start'..end') does not exceed (start..end)
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

    // log_max_count is technically a per-format static value so this shift could be a mux
    let check_lhs: BitInt<u64, 43> = Shifter::shl(CheckAdd::<u16, 16>::add(index, BitInt::<u8, 1>::constant(1)), initial.log_max_count).shrink();
    let check_rhs: BitInt<u64, 43> = Shifter::shl(old_elem_count, index_size_div).shrink();
    
    // check_lhs <= check_rhs iff check_rhs >= check_lhs
    match CheckSub::<u64, 43>::sub_overflow(check_rhs, check_lhs) {
        Some(_) => {
            // All good, keep going
        },
        None => {
            // check_lhs > check_rhs
            return Err(BadCaveatReason::Cav1IndexOob)
        }
    }

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
    // NOTE that this depends entirely on check_lhs <= check_rhs proven above
    // e.g. the trivial case index=1 elem_width_log2 = 64 is only possible if elem_count = (1 << 27), old_elem_width_log2 = 37, size_div = 0
    // e.g. check_lhs = 2 << 27, check_rhs = (1 << 27) << 0, check_lhs > check_rhs => pre-rejected
    // In fact, the only allowed index at that point is 0 for check_lhs = check_rhs = 1 << 27.
    let start_offset = CheckLeftShift::<u64, 64>::shl_proven_no_overflow(index, elem_width_log_2);

    // Make sure start_offset doesn't overflow
    let start = match Adder64::add_overflow(start, start_offset) {
        Some(start) => start,
        None => return Err(BadCaveatReason::Cav1StartOob)
    };
    
    Ok(PostCaveat1Range {
        elem_width_log_2, start
    })
}
fn apply_cav2(cav1: &PostCaveat1Range, range_x: Cav2RangeInt, range_y_minus_one: Cav2RangeInt) -> Result<PostCaveat2Range, BadCaveatReason> {
    // This caveat automatically shifts elem_width down by 2^9, so range_x (9-bit) and range_y (10-bit, max = 1 << 9) can select a subrange from the previous caveat.
    // More formally:
    //
    // width_log2'' = width_log2' - 9
    // (range_x, range_y) << (width_log2' - 9)     does not exit    (0, 1) << width_log2'
    //
    // shift up by 9
    //
    // (range_x[8:0], range_y[9:0]) << width_log2'      does not exit     (0, 1) << width_log2' + 9
    // range_x is an unsigned int therefore >= 0, so the bottom end doesn't matter
    // range_y is >= range_x
    //
    // range_y[9:0] << width_log2' <= 1 << (width_log2' + 9)
    // 
    // shift down by width_log2'
    //
    // range_y[9:0] <= 1 << 9 => true!
    //
    // a slight complication is that we're doing two things at once here - we're expanding all the (minus_10, shr_10) numbers to their full-width counterparts
    // so that we can select sub-kilobyte ranges.

    // Do the (y-1 >= x) check before finding (y-1)+1, because we don't want 0-length ranges which are the case where y=x => (y-1)<x
    let elem_count_minus_one = match CheckSub::<u16, 10>::sub_overflow(range_y_minus_one, range_x) {
        Some(elem_count_minus_one) => elem_count_minus_one,
        // (format!("caveat 2 range_x {range_x:?} is greater than range_y_minus_one {range_y_minus_one:?}"))
        None => return Err(BadCaveatReason::Cav2BadRange),
    };

    // We can actually discard range_y here
    let elem_count = CheckAdd::<u16, 10>::add(elem_count_minus_one, BitInt::<u8, 1>::constant(1));
    #[cfg(feature = "std")]
    assert_eq_hex!(elem_count.bound(), 1..=(1 << 9));

    // old elem_width_log_2_minus_10 = 0..=53,
    // old elem_width_log_2 = 10..=63
    // elem_width_log_2 = old elem_width_log_2 - 9
    // 
    // elem_width_log_2 = 1..=54
    //
    // range_x = 9 bits => top bit will get shifted up to position 63
    // elem_count is 10 bits through range_y so (elem_count << elen_width_log_2) will still hit the 64th bit.
    let (elem_count, elem_width_log_2, start_offset) = match CheckSub::<u8, 7>::sub_overflow(cav1.elem_width_log_2, BitInt::<u8, 7>::constant(9)) {
        Some(e) => {
            let elem_width_log_2 = e.shrink::<u8, 6>(); // old max = 64, new max = 55
            assert_eq!(elem_width_log_2.bound(), 0..=55);
            let start_offset = CheckLeftShift::<u64, 64>::shl(range_x, elem_width_log_2);
            (elem_count, elem_width_log_2, start_offset)
        },
        None => {           
            // cav1.elem_width_log_2 is too small to subdivide into 2^9 pieces.
            // e.g. if the range is 256 bytes long we don't have enough room for 512 pieces.
            // => We need to eliminate some bits of the range to fix the precision at 1-byte pieces - so if the range is 256, only use 8 bits of range_y and range_x.
            // This is effectively truncation.
            // Truncation usually requires a variable shift-right (i.e. extracting a bit range starting at a variable position), but we don't have a right-shifter - we only have a left-shifter.
            // Fortunately we have an upper limit on the truncation - it can be at most 9 (range_x = 0, range_y = 1 << 9, cav1_elem_width_log_2 = 0 -> new_range_x = 0, new_length = 1)
            // so we can take values range_x[8:0] and elem_count[9:0], treat them as fixed point numbers with 9 fractional bits (i.e. the integer portion is the maximum right-shift)
            // and then shift those fixed-point numbers *left* by (cav2_elem_width_log_2 + 9) (assuming cav2_elem_width_log_2 = -1 for a 1-bit right shift)
            // i.e. shift them left by cav1_elem_width_log_2(!)
            // and finally take the integer portion post-shift, which is a static window and thus cheap in hardware.
            
            // We actually have two quantities to right-shift: range_x and elem_count.
            // We can shift them in the same cycle, because they're small.
            // Consider 
            // (17 bits) 0'8 | range_x[8:0]
            // The window for the shifted_range_x is
            // (17 bits) shift_range_x[7:0] | X'9
            // (max left shift is 8, which takes one bit off)
            // which means shift_range_x[7:0] = 0'8          if left shift amt = 0
            // and         shift_range_x[7:0] = range_x[8:1] if left shift amt = 8 (max)
            //
            // we can attach elem_count on the end
            //
            // (35 bits) |       0'8      | elem_count'10 |        0'8      | range_x'9 |
            // with shift windows
            // (35 bits) |  shift_elem_count'9  |   X'9   | shift_range_x'8 |    X'9    |
            // which means
            // shift_elem_count[8:0] = 0'8 | elem_count[9] if left shift amt = 0
            // shift_elem_count[8:0] = elem_count[9:1]     if left shift amt = 8 (max)
            //
            // if desired extra 0s can pad the boundary between elem_count and range_x, as long as the X'9 between shift_elem_count and shift_range_x is extended by the same amount.
            // the maximum amount of extra padding is 64-35 = 29 bits.

            // depending on latencies/possible overlapping, it may be desirable to truncate range_x and range_y_minus_one *before* subtracting them and adding one to get elem_count.
            // this may also be desirable because it means the bottom bits of range_x and range_y are entirely meaningless, instead of the current situation where they can influence the calculation of elem_count.
            // additionally, this would bring back the length > 1 guarantee - precalculating elem_count means elem_count[9] could be zero and thus shift_elem_count can be zero.
            // anything that generates these capabilities should zero those bottom bits regardless.
            // TODO leaving this point until I check out the hardware impl.

            // actual proof that it doesn't go out:
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

            // If elem_width_log_2' == 8, we want the overall effect to be shifting range_x and elem_count down by 1, and setting elem_width_log_2'' to zero
            let cav1_elem_width_log_2: BitInt<u8, 4> = BitInt::new_bound(cav1.elem_width_log_2.val(), cav1.elem_width_log_2.min()..=8);

            // Shift up by (positive number = target + 9)
            let start_offset = CheckLeftShift::<u64, 17>::shl(range_x, cav1_elem_width_log_2);
            // Shift it down by a constant factor of 9, so the overall effect is a shift-right of (positive number - 9)
            let start_offset: BitInt<u16, 8> = const_shift_right(start_offset);

            // This shift can be merged with the above shift coz they're by the same value and both fit inside 64-bits
            let elem_count_shl = CheckLeftShift::<u32, 18>::shl(elem_count, cav1_elem_width_log_2);
            // Previously elem_count was 10-bits long. Now we know cav1_elem_width_log2 <= 8 we know it will be shifted down by at least 1, hence it's now 9 bits long.
            let elem_count: BitInt<u16, 9> = const_shift_right(elem_count_shl);
            (elem_count.expand(), BitInt::constant(0), start_offset.expand())
        }
    };

    // High-level assert that the overall length won't be too large
    // TODO new high-level assert that actually checks end'' doesn't go past end'.
    assert!(Shifter::shl(CheckAdd::<u16, 10>::add(range_y_minus_one, BitInt::<u8, 1>::constant(1)), elem_width_log_2).max() <= cav1.to_start_length().1.max());

    // This will never overflow on shift but the add could overflow if `start` is big enough.
    // This should never happen in practice because if start was large enough, cav1 would have hit the same issue.
    let start = match Adder64::add_overflow(cav1.start, start_offset) {
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

    use crate::{capability::{v2024_02::{cavs::{apply_cav1, apply_cav2, BadCaveatReason, ToStartLength}, AddrRange}, CapDecodeErr}, checked_bits::BitInt};

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
    
    // TODO use cav1 (0, 0) as an identity caveat? - it's only useful as a direct caveat when it's the identity too
    // TODO that complicates cav2 though :))))))))
    // TODO test this

    #[test]
    fn test_biggest_cav1_on_max_size() -> Result<(), CapDecodeErr> {
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
    fn test_biggest_cav1_on_smaller_size() -> Result<(), CapDecodeErr> {
        // Take a 1KiB addr range
        let initial = PartialEncodedInitialRange::new_test(
            10-10, // elem_width_log2_minus_10
            5, // log_max_count
            1, // max = (((implicit 1 << 26) | 0x3FF_FFFF (26 bits)) (27bits total)) + 1 (28 bits total, max = top bit set and rest are 0)
            0, // base_addr_shr_10
        );

        assert_eq_hex!(initial.to_addr_range()?, (0..(1<<10)).into(), "initial range wasn't correct");

        // cav1 with index_size_div 0 will try to make a 2^(10 + log_max_count - 0) = 2^15 range
        // TODO given that this is always the largest possible number maybe use it as an identity caveat?
        let cav1 = apply_cav1(&initial, BitInt::new(0), BitInt::new(0));
        assert_eq!(cav1.unwrap_err(), BadCaveatReason::Cav1IndexOob);
        // assert_eq_hex!(cav1.to_addr_range()?, (0..(1<<10)), "cav1 incorrectly shrunk the range");

        Ok(())
    }
    
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

        let cav2 = apply_cav2(&cav1, BitInt::new(0), BitInt::new(0x1_FF))?;
        assert_eq_hex!(cav2.to_addr_range()?, AddrRange::FULL_ADDR_SPACE, "cav2 incorrectly shrunk the range");

        // caveats can be enabled but all-zero
        // (technically this is for the purpose of permissions shrinking, and it's only possible for one caveat to shrink permissions i.e. be null, but for our purposes we want the maths to work)
        let cavs = Caveats::Two { index: BitInt::new(0), index_size_div: BitInt::new(0), range_x: BitInt::new(0), range_y_minus_one: BitInt::new(0x1_FF) };
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

        // At best we can shrink this by a factor of 2^9 => 2 byte range
        let cav2 = apply_cav2(&cav1, BitInt::new(0), BitInt::new(0))?;
        assert_eq_hex!(cav2.to_addr_range()?, (0..2).into(), "cav2 incorrectly shrunk the range");

        // caveats can be enabled but all-zero
        // (technically this is for the purpose of permissions shrinking, and it's only possible for one caveat to shrink permissions i.e. be null, but for our purposes we want the maths to work)
        let cavs = Caveats::Two { index: BitInt::new(0), index_size_div: BitInt::new(5), range_x: BitInt::new(0), range_y_minus_one: BitInt::new(0) };
        assert_eq_hex!(cavs.apply_to(&initial)?, (0..2).into());

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
        let cav2 = apply_cav2(&cav1, BitInt::new(0), BitInt::new(0x1_FF))?;
        assert_eq_hex!(cav2.to_addr_range()?, (0..1).into(), "cav2 incorrectly shrunk the range");

        let cavs = Caveats::Two { index: BitInt::new(0), index_size_div: BitInt::new(15), range_x: BitInt::new(0), range_y_minus_one: BitInt::new(0x1_FF) };
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


    // TODO test nonzero starting addresses

}