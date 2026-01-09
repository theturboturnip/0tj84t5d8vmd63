//! This capability format is mostly the same as the one presented in "So you want to design a capability", except the Capability Permissions Chain is used to describe which caveats are enabled, not what format the capability takes.
//! The two-caveat format is always used.
//! The B|C layout has changed to that presented in https://docs.google.com/spreadsheets/d/1iCPURHf2zLnvSDNQREE8upmmwlQl5h2x7yhd1dHRd8w/edit#gid=840091080 - 10-to-54-bit length, with use of implicit one-bit 
//! The initial length encoding is identical to 2024_02.
//! cav1 is identical to 2024_02.
//! cav2 is enlarged from a 2^9 base reduction in 2024_02 to a 2^14 base reduction in 2024_11 (+5 bits to x and y).

use core::marker::PhantomData;

#[cfg(feature = "std")]
use assert_hex::assert_eq_hex;

use bitutils::{bf, bits};

use crate::{checked_bits::{BitInt, CheckAdd}, crypto::Signaturer, util::u128_to_byte_array};

use self::cavs::{Caveats, PartialEncodedInitialRange, ToStartLength};

use super::{v2024::{perms::CapPermsChain, Cap2024}, AddrRange, CapDecodeErr, CapEncodeErr, CapPerms, Capability};

mod cavs;
mod rand;
#[cfg(test)]
mod test;

const MIN_INITIAL_LENGTH: u128 = 1 << 10;
// We're reusing the length curve from 2024_02, so it's possible to represent a length > 2^54.
// But the base caps out at 54 bits so TODO maybe don't allow that.
const MAX_INITIAL_LENGTH: u128 = 1 << 64;

#[derive(Debug, Clone)]
pub struct Cap2024_11<TSigner: Signaturer> {
    pub sig: u128,
    pub data: u128,
    pub cap_data: CapData,
    pub addr_range: AddrRange,
    pub signer: PhantomData<TSigner>
}
impl<TSigner: Signaturer> PartialEq for Cap2024_11<TSigner> {
    fn eq(&self, other: &Self) -> bool {
        self.sig == other.sig && self.data == other.data
    }
}
impl<TSigner: Signaturer> Cap2024 for Cap2024_11<TSigner> {
    type CapData = CapData;
    
    fn cap_data(&self) -> &Self::CapData {
        &self.cap_data
    }

    fn cap_perms_chain(&self) -> CapPermsChain {
        self.cap_data.perms_chain
    }

    fn cap_perms(&self) -> CapPerms {
        self.cap_data.perms_chain.into()
    }

    fn secret_key_id(&self) -> u32 {
        self.cap_data.secret_key_id
    }

    fn new(secret: u128, secret_key_id: u32, base: u64, length: u128, perms: CapPerms) -> Result<Self, CapEncodeErr> {
        let initial_fullyencoded = EncodedInitialRange::pack_base_and_length_exact(&AddrRange::base_len(base, length))?;
        let cap_data = CapData {
            perms_chain: CapPermsChain::from_root(perms),
            secret_key_id,
            initial: initial_fullyencoded.partial_decode(),
            initial_fullyencoded,
            caveats: Caveats::Zero,
        };
        let data: u128 = (&cap_data).into();
        
        Ok(Self {
            sig: TSigner::sign(&u128_to_byte_array(data), secret),
            data,
            cap_data,
            addr_range: AddrRange::base_len(base, length),
            signer: PhantomData::default()
        })
    }

    fn new_inexact(secret: u128, secret_key_id: u32, base: u64, length: u128, perms: CapPerms) -> Result<Self, CapEncodeErr> {
        let (initial_fullyencoded, addr_range) = EncodedInitialRange::pack_base_and_length(&AddrRange::base_len(base, length))?;
        let cap_data = CapData {
            perms_chain: CapPermsChain::from_root(perms),
            secret_key_id,
            initial: initial_fullyencoded.partial_decode(),
            initial_fullyencoded,
            caveats: Caveats::Zero,
        };
        let data: u128 = (&cap_data).into();
        
        Ok(Self {
            sig: TSigner::sign(&u128_to_byte_array(data), secret),
            data,
            cap_data,
            addr_range,
            signer: PhantomData::default()
        })
    }


    fn from_bits(sig: u128, data: u128) -> Result<Self, CapDecodeErr> {
        let cap_data: CapData = data.try_into()?;
        Ok(Self {
            sig,
            data,
            addr_range: cap_data.encoded_range()?,
            cap_data,
            signer: PhantomData::default(),
        })
    }

    fn refine_using_cav1_towards(&self, new_perms: Option<CapPerms>, target: AddrRange) -> Result<Self, CapEncodeErr> {
        if !self.addr_range.encompasses(&target) {
            return Err(CapEncodeErr::UnrepresentableCaveat);
        }
        if target.len() > MAX_INITIAL_LENGTH.into() {
            return Err(CapEncodeErr::UnrepresentableCaveat);
        }

        // Caveat 1 is a pow2 index caveat which can shift down by 2^15.
        // The outcome of this is a pow2 range of length 2^n where base is aligned to at least n.
        // We want the smallest pow2 2^n which encompasses (base rounded down to 2^n) to (base + len rounded up to 2^n)
        // Constraints:
        // - 2^n >= len
        // - round(base, n) <= base
        // The properties of the initial range are such that for any requested length >= 2^10, it will fall in the range [2^(rounddown(log2(initial_len))), 2^(rounddown(log2(initial_len))+1)]
        // such that initial_base is aligned to n < rounddown(log2(initial_len)), and initial_base <= base, and initial_len >= len
        // So rounding to a specific power of two doesn't help us unless we're going smaller than 2^10.
        // It is impossible for there to be a smaller power-of-two to pack down to - NOTE: unless the power-of-two brings the base closer??? No, I don't think it'd bring it closer by the magnitude of a pow2.
        // BUT it is still important to try to pack cav1 down.
        // If the initial range uses one of the four no-implicit-one ranges (2^10-2^15, 2^22-2^29, 2^36-2^43, 2^50-2^57) the maximum pow2 (which cav1's size_div is relative to) will always be the upper end of the range.
        // If we simply made cav1 the identity, i.e. size_div=0, the cav1 range would use a pow2=that upper end.
        // The accessible range isn't expanded if cav1 represents a larger range than initial resource, but it puts cav2 at a disadvantage because its size_div is relative to the cav1 range.
        // For example: if the initial range is 2^23 in length, cav1's size_div is implicitly relative to 2^29.
        // If we take cav1 size_div = 0, cav1 represents a 2^29 range and cav2's slices are in terms of 2^20.
        // But if we take cav1 size_div = 6, cav1 represents a 2^23 range and cav2's slices are in terms of 2^14.
        // Another example:
            // A truly Identity cav1 is actually unhelpful, because it doesn't bring down the pow2 size for the next level.
            // Instead, allow cav1 to reduce to a pow2 outside the bounds (e.g. if the initial resource is 3 * 2^10, allow a 2^12 cav1)
            // and treat that as not reducing the bounds at all - but lowering the size_div for cav2 to 2^12 instead of 2^15 (2^10 * 2^log_max_count)
            // so that cav2 can go further.

        // cav1 represents an aligned pow2 region *relative to whatever base is specified in the initial resource*.
        let base_rel_initial = target.base() - self.addr_range.base();
            
        // Find the smallest pow2 the cav1 can possibly limit this to.
        let max_pow2_of_cav1 = self.cap_data.initial.elem_width_log2_minus_10.val() + 10 + self.cap_data.initial.log_max_count.val();
        let min_pow2_of_cav1 = max_pow2_of_cav1.saturating_sub(15);

        // Select a size_div by shifting (intended base - initial base) and (intended top - initial base) down by that smallest pow2.
        // At most (intended top - intended base) can be 1 << max_pow2_of_cav1,
        // and thus shifting it down by min_pow2_of_cav1 will result in a number which is 1 << (max_pow2_of_cav1 - (max_pow2_of_cav1 - 15))
        // i.e. max((intended top - intended base) >> min_pow2_of_cav1) = 1 << 15
        // initial base is always <= intended base, => (intended top - initial base) may be LARGER. wait a minute!
        // TODO intended base is only larger than initial base by up the 2^base_align. base_align is always <= length_align... can this tie into the min_pow2 here?
        // 
        // If the values are equal, we can select the maximum size_div.
        // Otherwise, shift them down further one bit at a time (equivalent to reducing size_div by 1) until they match.
        // These should always fit in a u16 TODO just don't bother. We can do 64-bit shifts
        let base_top16: u16 = (base_rel_initial >> min_pow2_of_cav1).try_into().unwrap();
        assert!(base_top16 <= (1 << 15));
        let top_excl_top16: u16 = ((base_rel_initial + (target.len() as u64)) >> min_pow2_of_cav1).try_into().unwrap();
        assert!(top_excl_top16 <= (1 << 15));
        let top_top16: u16 = top_excl_top16 - 1;
        assert!(top_top16 <= (1 << 15));

        let mut selected_index = 0;
        let mut selected_size_div = 0;
        for fifteen_minus_size_div in 0..=15 {
            // cav1 behaves as if we split the initial resource into slices of a specific pow2, and select one.
            // We want to select the smallest pow2 (the largest size_div, thus the smallest fifteen_minus_size_div)
            // where the intended base and intended top both fit in the same slice.
            // We evaluate this by checking the slice *index* the base and top would reside in, and increasing fifteen_minus_size_div until they are equal.
            let base_index = base_top16 >> fifteen_minus_size_div;
            let top_index = top_top16 >> fifteen_minus_size_div;
            if base_index == top_index {
                selected_index = base_index;
                selected_size_div = 15 - fifteen_minus_size_div;
                break
            }
        }

        // If this happens to exceed the initial range length, that's OK.
        // The accessible range will still be constrained to the initial resource,
        // but cav2 will have an optimal starting point.
        // TODO if cav1 has zero benefit, allow not setting it at all?
        self.with_cav1_size_div(new_perms, selected_index, selected_size_div)
    }

    /// Take the Nth pow2-width slice of the provided range, optionally reducing access permissions.
    /// 
    /// Specifies the pow2-width directly.
    fn with_cav1(&self, new_perms: Option<CapPerms>, index: u16, pow2: u8) -> Result<Self, CapEncodeErr> {
        let cap_data = self.cap_data.clone();
        match cap_data.caveats {
            Caveats::Zero => {
                let max_pow2_of_cav1 = cap_data.initial.elem_width_log2_minus_10.val() + 10 + cap_data.initial.log_max_count.val();
                if pow2 > max_pow2_of_cav1 {
                    return Err(CapEncodeErr::UnrepresentableCaveat)
                }
                self.with_cav1_size_div(new_perms, index, max_pow2_of_cav1 - pow2)
            }
            _ => Err(CapEncodeErr::NoCaveatsLeft) // cav1 already taken
        }
    }

    /// Take the Nth pow2-width slice of the provided range, optionally reducing access permissions.
    /// 
    /// Specifies the pow2-width relative to the initial resource.
    fn with_cav1_size_div(&self, new_perms: Option<CapPerms>, index: u16, size_div: u8) -> Result<Self, CapEncodeErr> {
        let cap_data = self.cap_data.clone();
        match cap_data.caveats {
            Caveats::Zero => {
                let perms_chain = match new_perms {
                    Some(new_perms) => cap_data.perms_chain.try_add_caveat(new_perms)?,
                    // If no new_perms are supplied, use the old ones
                    None => cap_data.perms_chain.try_add_caveat(cap_data.perms_chain.into())?,
                };
                if size_div > 15 {
                    return Err(CapEncodeErr::UnrepresentableCaveat)
                }
                let caveats = Caveats::One { index: BitInt::new(index), index_size_div: BitInt::new(size_div) };
                // test if index is oob
                let addr_range = caveats.apply_to(&cap_data.initial)?;
                let cap_data = CapData {
                    perms_chain,
                    caveats,
                    ..cap_data
                };

                let data: u128 = (&cap_data).into();
        
                Ok(Self {
                    sig: TSigner::sign(&u128_to_byte_array(data), self.sig),
                    data,
                    cap_data,
                    addr_range,
                    signer: PhantomData::default()
                })
            }
            _ => Err(CapEncodeErr::NoCaveatsLeft) // cav1 already taken
        }
    }

    // TODO TEST THESE

    fn refine_using_cav2_towards(&self, new_perms: Option<CapPerms>, target: AddrRange) -> Result<Self, CapEncodeErr> {
        if !self.addr_range.encompasses(&target) {
            return Err(CapEncodeErr::UnrepresentableCaveat);
        }
        if target.len() > MAX_INITIAL_LENGTH.into() {
            return Err(CapEncodeErr::UnrepresentableCaveat);
        }

        // We now have either
            // - a null cav1 over an initial non-pow2 range surrounding (base, base+len)
            // - a pow2 cav1 surrounding (base, base+len)
        let cav1_pow2 = match self.cap_data.caveats {
            Caveats::Zero => return Err(CapEncodeErr::InvalidCaveat), // TODO better signal that cav1 isn't filled in yet
            Caveats::One { index_size_div, .. } => {
                self.cap_data.initial.elem_width_log2_minus_10.val() + 10 + self.cap_data.initial.log_max_count.val() - index_size_div.val()
            },
            Caveats::Two { .. } => return Err(CapEncodeErr::NoCaveatsLeft) // cav2 already taken or cav1 not taken yet
        };

        let (range_x, range_y) = if cav1_pow2 >= 14 {
            // In the normal case the region is split into 16,384 sub-regions and we take a contiguous set
            let slice_pow2 = cav1_pow2 - 14;

            // cav1_range encompasses base and base + len
            // =>  (base - cav1_range.base())       <  2^cav1_pow2
            // and (base + len - cav1_range.base()) <= 2^cav1_pow2
            // => when shifted down by (cav1_pow2 - 14), they are both <=2^14

            // If we shift bits out of the bottom, the resulting range will be rounded down
            // => check for that and bump up the range manually.
            // This will still be <= 2^14 because the bottom bits of (top_offset) will only be set if it is not a pow2,
            // thus will only ever happen if top_offset is strictly < 2^cav1_pow2
            // thus will only ever happen if (top_offset >> (cav1_pow2 - 14)) is strictly < 2^14
            // so incrementing will only ever push up to exactly 2^14.
            // The rounding down behaviour is what we want for the base.

            // This used to truncate top_offset to a u16, but this broke the C implementation.
            // It doesn't make sense to truncate top_offset here, because if the length is large cav1_pow2 will be >16
            // and so the top_offset will not fit.
            // I suppose I hadn't tested it before :#
            let top_offset = target.base() + (target.len() as u64) - self.addr_range.base();
            let mut range_y = (top_offset >> slice_pow2) as u16;
            if ((range_y as u64) << slice_pow2) < top_offset {
                range_y = range_y + 1;
            }
            assert!(range_y <= (1 << 14));

            (
                ((target.base() - self.addr_range.base()) >> slice_pow2) as u16,
                range_y
            )
        } else {
            // If cav1_pow2 is already smaller than 2^14, we can go down to the byte level *but* range_x range_y need to be *left* aligned
            // (this is kind of a dumb rule but it's what i've got.)
            // e.g. if cav1_pow2 is 2^8, we can select any bytes from 0..256 - but multiply range_x and range_y by 2 so that byte 256 is represented by range_y = 512.

            // cav1_range encompasses base and base + len
            // =>  (base - cav1_range.base())       <  2^cav1_pow2
            // and (base + len - cav1_range.base()) <= 2^cav1_pow2
            // => when shifted left by (14 - cav1_pow2), they are both <=2^14
            (
                ((target.base() - self.addr_range.base()) << (14 - cav1_pow2)) as u16,
                ((target.base() + (target.len() as u64) - self.addr_range.base()) << (14 - cav1_pow2)) as u16
            )
        };

        // TODO should this always refine, even if it's unnecessary?
        // if new_perms.is_none() && range_x == 0 && range_y == 1 << 14 {
        self.with_cav2(new_perms, range_x, range_y)
    }

    /// Take a subrange of the provided range in 16,384 increments, optionally reducing access permissions
    fn with_cav2(&self, new_perms: Option<CapPerms>, range_x: u16, range_y: u16) -> Result<Self, CapEncodeErr> {
        let cap_data = self.cap_data.clone();
        match cap_data.caveats {
            Caveats::Zero => Err(CapEncodeErr::InvalidCaveat), // TODO better signal that cav1 isn't filled in yet
            Caveats::One { index, index_size_div } => {
                let perms_chain = match new_perms {
                    Some(new_perms) => cap_data.perms_chain.try_add_caveat(new_perms)?,
                    // If no new_perms are supplied, use the old ones
                    None => cap_data.perms_chain.try_add_caveat(cap_data.perms_chain.into())?,
                };
                if range_y <= range_x {
                    return Err(CapEncodeErr::UnrepresentableCaveat)
                }
                if range_y == 0 {
                    return Err(CapEncodeErr::UnrepresentableCaveat)
                }
                let caveats = Caveats::Two { index, index_size_div, range_x: BitInt::new(range_x), range_y_minus_one: BitInt::new(range_y - 1) };
                // test if range is oob
                let addr_range = caveats.apply_to(&cap_data.initial)?;
                let cap_data = CapData {
                    perms_chain,
                    caveats,
                    ..cap_data
                };

                let data: u128 = (&cap_data).into();
        
                Ok(Self {
                    sig: TSigner::sign(&u128_to_byte_array(data), self.sig),
                    data,
                    cap_data,
                    addr_range,
                    signer: PhantomData::default()
                })
            }
            _ => Err(CapEncodeErr::NoCaveatsLeft) // cav2 already taken or cav1 not taken yet
        }
    }

    fn cap_perms_bitfield_from_raw(data: u128) -> Result<CapPermsChain, CapDecodeErr> {
        let bitfield = CapBitfield::new(data);
        (bitfield.perms() as u8).try_into()
    }
}
impl<TSigner: Signaturer> Capability for Cap2024_11<TSigner> {
    type Secret = u128;
    type Signature = u128;

    fn requested_range(&self) -> Result<(AddrRange, CapPerms), CapDecodeErr> {
        Ok((self.addr_range, self.cap_data.perms_chain.into()))
    }

    fn signature(&self) -> Self::Signature {
        self.sig
    }

    fn expected_signature(&self, secret: Self::Secret) -> Self::Signature {
        let mut sig = secret;
        
        let mut rehash_sig = |data: &CapData| {
            sig = TSigner::sign(&u128_to_byte_array(data.into()), sig);
        };
        
        match self.cap_data.caveats {
            Caveats::Zero => rehash_sig(&self.cap_data),
            Caveats::One { .. } => {
                rehash_sig(&CapData {
                    perms_chain: self.cap_data.perms_chain.back_in_time().unwrap(),
                    caveats: Caveats::Zero,
                    ..self.cap_data.clone()
                });
                rehash_sig(&self.cap_data);
            },
            Caveats::Two { index, index_size_div, .. } => {
                rehash_sig(&CapData{
                    perms_chain: self.cap_data.perms_chain.root(),
                    caveats: Caveats::Zero,
                    ..self.cap_data.clone()
                });
                rehash_sig(&CapData{
                    perms_chain: self.cap_data.perms_chain.back_in_time().unwrap(),
                    caveats: Caveats::One {
                        index,
                        index_size_div,
                    },
                    ..self.cap_data.clone()
                });
                rehash_sig(&self.cap_data);
            },
        }

        sig
    }
}

// Cap2024_11 EDIT
// Reduced b_c by 10 bits, increased range_x and range_y by 5 bits each
bf!(pub CapBitfield[u128] {
    encoded_elem_width: 0:4,
    b_c: 5:53,
    index_size_div: 54:57,
    index: 58:72,
    range_x: 73:86,
    // range_y_minus_one = "the index of the final block we use",
    // calculating the actual length requires range_y to represent "the index of the first block we don't"
    // (so the start address of that block represents the end address of the final valid block)
    // TODO consider whether this should be named "range_y_minus_one" or if the caveat code should be reasoning about "range_y_plus_one" to find the end of the range.
    range_y_minus_one: 87:100,
    secret_key_id: 101:123, // 23 bits
    perms: 124:127
});
// Cap2024_11 EDIT END

#[derive(Debug, Clone)]
pub struct EncodedInitialRange {
    encoded_elem_width: BitInt<u8, 5>, // 5 bits
// Cap2024_11 EDIT
// Reduced b_c by 10 bits
    b_c: BitInt<u64, 49>, // 49 bits
// Cap2024_11 EDIT END
}

// Cap2024_11 EDIT
// Reduced b_c by 10 bits, increased range_x and range_y by 5 bits each
impl EncodedInitialRange {
    // TODO get_representable_range?

    fn pack_base_and_length(r: &AddrRange) -> Result<(Self, AddrRange), CapEncodeErr> {
        let length = {
            let length = r.len();
            if length > MAX_INITIAL_LENGTH.into() {
                return Err(CapEncodeErr::UnrepresentableBaseRange);
            }
            if length == 0 {
                return Err(CapEncodeErr::UnrepresentableBaseRange);
            }
            if r.start >= (1 << 54) {
                return Err(CapEncodeErr::UnrepresentableBaseRange);
            }

            // Minimum length == 2^10. If smaller than that, just take that.
            // TODO don't do that!!! that means we'll try to round it up!
            if length <= MIN_INITIAL_LENGTH.into() {
                MIN_INITIAL_LENGTH
            } else {
                length
            }
        };

        let (implicit_1, elem_width_log_2) = {
            // When equal to minimum length, shortcut.
            if length <= MIN_INITIAL_LENGTH {
                (false, 10)
            } else {
                // Each given elem_width captures a range up to 2^(elem_width + elem_count_bits) *inclusive*, because elem_count is stored with a +1 offset.
                // In most cases there are two possible elem_widths to choose that could represent the same length - one with fewer elem_count bits and implicit_1 = false, 
                // and another with more elem_count bits and implicit_1 = true.
                // We want to tend downwards, because smaller elem_widths can be shrunk further with caveats.

                // For implicit_1 = true, the range is [2^(elem_width+elem_count_bits) *exclusive*, 2^(elem_width+elem_count_bits+1) *inclusive*]
                // For implicit_1 = false, applicable to elem_width = 10, 17, 24, 31; the range is [2^0 inclusive, 2^(elem_width+elem_count_bits) inclusive]
                // In both cases the top is an exact power-of-two that's inclusive, so we can use floor(log2(length - 1)) to directly determine the range 

                let log_len_minus1 = (length - 1).ilog2() as u8;
                match log_len_minus1 {
                    10..=14 => (false, 10), // if (length-1) >= 2^15 then it's represented by the next one
                    15..=21 => (true, log_len_minus1 - 5), // 10..=16
                    22..=28 => (false, 17), // if length == 2^22 it is represented by the previous one, if length == 2^22+1*block it's represented with this
                    29..=35 => (true, log_len_minus1 - 12), // 17..=23
                    36..=42 => (false, 24),
                    43..=49 => (true, log_len_minus1 - 19), // 24..=30
                    50..=56 => (false, 31),
                    57..=63 => (true, log_len_minus1 - 26), // 31..=37
                    // The maximum length is 2^64, so the maximum log(length - 1) is 63.
                    _ => unreachable!()
                }
            }
        };
        // We encode elem_width with a specific curve which allows easy lookups of whether the implicit 1 is set
        let encoded_elem_width: BitInt<u8, 5> = match (implicit_1, elem_width_log_2) {
            (false, 10) => BitInt::new(31),
            (true, 10..=16) | (false, 17) => BitInt::new(elem_width_log_2 - 10),
            (true, 17..=23) | (false, 24) => BitInt::new(elem_width_log_2 - 9),
            (true, 24..=30) | (false, 31) => BitInt::new(elem_width_log_2 - 8),
            (true, 31..=37) => BitInt::new(elem_width_log_2 - 7),
            _ => unreachable!()
        };
        let count_bits: u8 = match elem_width_log_2 {
            10..=16 => 5,
            17..=23 => 12,
            24..=30 => 19,
            31..=37 => 26,
            _ => unreachable!()
        };
        let base_bits = 49 - count_bits;
        let count = {
            // elem_width_log_2 is 10 at minimum, length is (1 << 64) at maximum, => length >> elem_width_log_2 is at most (1 << 54) which fits into 64bits.
            // (count + 1) will also always fit into 64 bits.
            let count = length >> elem_width_log_2;
            // If we shifted any one-bits out
            if (count << elem_width_log_2) < length {
                // Add one to the count
                (count + 1) as u64
            } else {
                count as u64
            }
        };
        let encoded_count = if implicit_1 {
            (count - 1) & (!(1 << count_bits))
        } else {
            count - 1
        };
        let encoded_base = {
            const MAX_REPRSENTABLE_BASE_BITS: u8 = 54;
            let encoded_base = r.start >> (MAX_REPRSENTABLE_BASE_BITS - base_bits);
            // If we truncated the start...
            let new_start = encoded_base << (MAX_REPRSENTABLE_BASE_BITS - base_bits);
            if new_start < r.start {
                // ...redo the process. We've slightly increased the length, which means in edge cases we might move across a quadrant to a higher base-precision and thus do it again,
                // but it won't recurse beyond that.
                // TODO this will have issues when length = 1 << 64, that can't be rounded up
                return Self::pack_base_and_length(&AddrRange::start_end(new_start, r.end))
            }
            encoded_base
        };

        // The moment of truth: check the encoded count and base are within their respective bounds
        // TODO do a more formal proof that encoded_count will fit. I'm not comfortable leaving it like this but I'm pretty sure it's fine
        #[cfg(feature = "std")] {
            assert_eq_hex!(encoded_count & !(u64::MAX << count_bits), encoded_count);
            assert_eq_hex!(encoded_base & !(u64::MAX << base_bits), encoded_base);
        }

        let b_c = (encoded_base << count_bits) | encoded_count;
        // Check b_c is 49 bits long
        #[cfg(feature = "std")]
        assert_eq_hex!(b_c & (u64::MAX >> (64-49)), b_c);

        let s = Self {
            encoded_elem_width,
            b_c: BitInt::new(b_c),
        };

        let new_range = s.decode().unwrap();
        if !new_range.encompasses(r) {
            panic!("Initial range pack algorithm failure: computed range {:x?} doesn't encompass requested range {:x?}", new_range, r)
        }
        Ok((s, new_range))
    }

    fn pack_base_and_length_exact(r: &AddrRange) -> Result<Self, CapEncodeErr> {
        let (packed, new_range) = Self::pack_base_and_length(r)?;
        if (&new_range) == r {
            Ok(packed)
        } else {
            Err(CapEncodeErr::UnrepresentableBaseRange)
        }
    }

    fn partial_decode(&self) -> PartialEncodedInitialRange {
        let encoded_elem_width = self.encoded_elem_width;
        let encoded_elem_width_bot = bits!(encoded_elem_width.val(), 0:2) as u8;
        let encoded_elem_width_top = bits!(encoded_elem_width.val(), 3:4) as u8;

        let implicit_1 = if encoded_elem_width_bot == 0b111 { 0 } else { 1 };
        let elem_width_log2_minus_10 = match encoded_elem_width_top {
            // min value = (0b00 | 0b000) = 0
            0b00 => encoded_elem_width.val(),
            0b01 => encoded_elem_width.val() - 1,
            0b10 => encoded_elem_width.val() - 2,
            0b11 => if encoded_elem_width_bot == 0b111 {
                0
            } else {
                // max value = (0b11 | 0b110) - 3 = 27
                encoded_elem_width.val() - 3
            },
            _ => unreachable!()
        };

        let b_c = self.b_c;
        // need to map (31, 0..=6) => 0, (7, 8..=14) => 1, (15, 16..=22) => 2, (23, 24..=30) => 3
        let c_bits_selector = if encoded_elem_width_bot == 0b111 {
            (encoded_elem_width_top + 1) & 0b11
        } else {
            encoded_elem_width_top
        };
        let (base_addr_shr_10, elem_count, log_max_count) = match c_bits_selector {
            0b00 => (
                BitInt::new(bits!(b_c.val(), 5:48)), // << 10 - 10
                BitInt::<u32, 27>::new((implicit_1 << 5) | bits!(b_c.val(), 0:4) as u32),
                BitInt::<u8, 5>::new_manual(5 + implicit_1 as u8, 5, 27)
            ),
            0b01 => (
                BitInt::new(bits!(b_c.val(), 12:48) << 7), // 17 - 10
                BitInt::new((implicit_1 << 12) | bits!(b_c.val(), 0:11) as u32),
                BitInt::<u8, 5>::new_manual(12 + implicit_1 as u8, 5, 27)
            ),
            0b10 => (
                BitInt::new(bits!(b_c.val(), 19:48) << 14), // 24 - 10
                BitInt::new((implicit_1 << 19) | bits!(b_c.val(), 0:18) as u32),
                BitInt::<u8, 5>::new_manual(19 + implicit_1 as u8, 5, 27)
            ),
            0b11 => (
                BitInt::new(bits!(b_c.val(), 26:48) << 21), // 31 - 10
                BitInt::new((implicit_1 << 26) | bits!(b_c.val(), 0:25) as u32),
                BitInt::<u8, 5>::new_manual(26 + implicit_1 as u8, 5, 27)
            ),
            _ => unreachable!()
        };
        let elem_count = CheckAdd::<u32, 28>::add(elem_count, BitInt::<u8, 1>::constant(1));
        #[cfg(feature = "std")] {
            assert_eq_hex!(elem_count.max(), 1 << 27); // If the top bit of elem_count is set, the others won't be.
            assert_eq_hex!(log_max_count.max(), 27); // Therefore the max(log(max(count))) will also be 27, despite (count) being a 28-bit number.
        }

        PartialEncodedInitialRange {
            elem_width_log2_minus_10: BitInt::new_manual(elem_width_log2_minus_10, 0, 27), // The maximum possible value should be 27
            log_max_count,
            elem_count,
            base_addr_shr_10,
        }
    }

    fn decode(&self) -> Result<AddrRange, CapDecodeErr> {
        self.partial_decode().to_addr_range().map_err(Into::into)
    }
}
// Cap2024_11 EDIT END


#[derive(Debug, Clone)]
pub struct CapData {
    pub perms_chain: CapPermsChain,

    pub secret_key_id: u32,

    pub initial_fullyencoded: EncodedInitialRange,
    pub initial: PartialEncodedInitialRange,

    pub caveats: Caveats,
}
impl CapData {
    pub fn encoded_range(&self) -> Result<AddrRange, CapDecodeErr> {
        self.caveats.apply_to(&self.initial).map_err(Into::into)
    }
}
impl TryFrom<u128> for CapData {
    type Error = CapDecodeErr;

    fn try_from(value: u128) -> Result<Self, Self::Error> {
        let cap_bits = CapBitfield::new(value);

        let perms_chain: CapPermsChain = (cap_bits.perms() as u8).try_into()?;

        let secret_key_id = cap_bits.secret_key_id() as u32;

        let range_y_minus_one = BitInt::new(cap_bits.range_y_minus_one() as u16);
        let range_x = BitInt::new(cap_bits.range_x() as u16);
        let index = BitInt::new(cap_bits.index() as u16);
        let index_size_div = BitInt::new(cap_bits.index_size_div() as u8);

        let initial_fullyencoded = EncodedInitialRange {
            encoded_elem_width: BitInt::new(cap_bits.encoded_elem_width() as u8),
            b_c: BitInt::new(cap_bits.b_c() as u64),
        };
        let initial = initial_fullyencoded.partial_decode();

        let caveats = match perms_chain.n_cavs() {
            0 => {
                if index.val() != 0 || index_size_div.val() != 0 || range_x.val() != 0 || range_y_minus_one.val() != 0 {
                    return Err(CapDecodeErr::UnexpectedCaveat)
                }
                Caveats::Zero
            },
            1 => {
                if range_x.val() != 0 || range_y_minus_one.val() != 0 {
                    return Err(CapDecodeErr::UnexpectedCaveat)
                }
                Caveats::One { index, index_size_div }
            },
            2 => {
                Caveats::Two { index, index_size_div, range_x, range_y_minus_one }
            },
            _ => unreachable!()
        };

        let s = Self {
            perms_chain,
            secret_key_id,
            initial_fullyencoded,
            initial,
            caveats,
        };
        Ok(s)
    }
}
impl From<&CapData> for u128 {
    fn from(value: &CapData) -> Self {
        let mut bf = CapBitfield::new(0);

        bf.set_perms(value.perms_chain.encode() as u128);
        bf.set_secret_key_id(value.secret_key_id as u128);

        match value.caveats {
            Caveats::Zero => {
                bf.set_index(0);
                bf.set_index_size_div(0);
                bf.set_range_x(0);
                bf.set_range_y_minus_one(0);
            },
            Caveats::One { index, index_size_div } => {
                bf.set_index(index.val() as u128);
                bf.set_index_size_div(index_size_div.val() as u128);
                bf.set_range_x(0);
                bf.set_range_y_minus_one(0);
            },
            Caveats::Two { index, index_size_div, range_x, range_y_minus_one } => {
                bf.set_index(index.val() as u128);
                bf.set_index_size_div(index_size_div.val() as u128);
                bf.set_range_x(range_x.val() as u128);
                bf.set_range_y_minus_one(range_y_minus_one.val() as u128);
            },
        }

        bf.set_b_c(value.initial_fullyencoded.b_c.val() as u128);
        bf.set_encoded_elem_width(value.initial_fullyencoded.encoded_elem_width.val() as u128);

        bf.val
    }
}
