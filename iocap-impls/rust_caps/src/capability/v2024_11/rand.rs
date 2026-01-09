use core::u64;

use bitutils::bits;
use rand::prelude::*;

use crate::{capability::{v2024::{perms::{CapPermsChain, ALL_0CAV_CAP_PERMS_CHAINS, ALL_0_OR_1CAV_CAP_PERMS_CHAINS, ALL_1CAV_CAP_PERMS_CHAINS, ALL_2CAV_CAP_PERMS_CHAINS}, rand::{choose_from, CapTestCase, ChooseRand, RandCap}, Cap2024}, AddrRange, CapDecodeErr, CapEncodeErr, Capability}, crypto::Signaturer, util::u128_to_byte_array};

use super::{cavs::ToStartLength, Cap2024_11, CapBitfield, CapData};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum Cap2024_11_EdgeCase {
    // All_Signature_OffByOne,
    // All_Signature_Outdated,
    Cav1_NonZeroWhenUnused,
    Cav1_TooBig_IndexNonZero_Invalid,
    Cav1_TooBig_IndexZero_Valid,
    Cav2_NonZeroWhenUnused_Invalid,
    Cav2_YSmallerThanX_Invalid,
    Cav1_TooBig_IndexZero_Cav2_InBounds_Valid,
    Cav1_TooBig_IndexZero_Cav2_ExceedsIdentity_Invalid,
    // Cav1_PushStartOutsideLimit_Invalid,
    // Cav2_PushStartOutsideLimit_Invalid,
    // Cav2_IgnoredBitsOfXYAreSet_Valid,
    // TODO edge cases for subnormal initial ranges?
    Cav2_ShrinksToZero_Valid,
}
impl From<Cap2024_11_EdgeCase> for &'static core::ffi::CStr {
    fn from(value: Cap2024_11_EdgeCase) -> Self {
        match value {
            // Cap2024_11_EdgeCase::All_Signature_OffByOne => c"Cap2024_11_EdgeCase::All_Signature_OffByOne",
            // Cap2024_11_EdgeCase::All_Signature_Outdated => c"Cap2024_11_EdgeCase::All_Signature_Outdated",
            Cap2024_11_EdgeCase::Cav1_NonZeroWhenUnused => c"Cap2024_11_EdgeCase::Cav1_NonZeroWhenUnused",
            Cap2024_11_EdgeCase::Cav1_TooBig_IndexNonZero_Invalid => c"Cap2024_11_EdgeCase::Cav1_TooBig_IndexNonZero_Invalid",
            Cap2024_11_EdgeCase::Cav1_TooBig_IndexZero_Valid => c"Cap2024_11_EdgeCase::Cav1_TooBig_IndexZero_Valid",
            Cap2024_11_EdgeCase::Cav2_NonZeroWhenUnused_Invalid => c"Cap2024_11_EdgeCase::Cav2_NonZeroWhenUnused_Invalid",
            Cap2024_11_EdgeCase::Cav2_YSmallerThanX_Invalid => c"Cap2024_11_EdgeCase::Cav2_YSmallerThanX_Invalid",
            Cap2024_11_EdgeCase::Cav1_TooBig_IndexZero_Cav2_InBounds_Valid => c"Cap2024_11_EdgeCase::Cav1_TooBig_IndexZero_Cav2_InBounds_Valid",
            Cap2024_11_EdgeCase::Cav1_TooBig_IndexZero_Cav2_ExceedsIdentity_Invalid => c"Cap2024_11_EdgeCase::Cav1_TooBig_IndexZero_Cav2_ExceedsIdentity_Invalid",
            // Cap2024_11_EdgeCase::Cav1_PushStartOutsideLimit_Invalid => c"Cap2024_11_EdgeCase::Cav1_PushStartOutsideLimit_Invalid",
            // Cap2024_11_EdgeCase::Cav2_PushStartOutsideLimit_Invalid => c"Cap2024_11_EdgeCase::Cav2_PushStartOutsideLimit_Invalid",
            // Cap2024_11_EdgeCase::Cav2_IgnoredBitsOfXYAreSet_Valid => c"Cap2024_11_EdgeCase::Cav2_IgnoredBitsOfXYAreSet_Valid",
            Cap2024_11_EdgeCase::Cav2_ShrinksToZero_Valid => c"Cap2024_11_EdgeCase::Cav2_ShrinksToZero_Valid",
        }
    }
}
const CAP2024_11_EDGE_CASES: [Cap2024_11_EdgeCase; 8] = [
    // Cap2024_11_EdgeCase::All_Signature_OffByOne,
    // Cap2024_11_EdgeCase::All_Signature_Outdated,
    Cap2024_11_EdgeCase::Cav1_NonZeroWhenUnused,
    Cap2024_11_EdgeCase::Cav1_TooBig_IndexNonZero_Invalid,
    Cap2024_11_EdgeCase::Cav1_TooBig_IndexZero_Valid,
    Cap2024_11_EdgeCase::Cav2_NonZeroWhenUnused_Invalid,
    Cap2024_11_EdgeCase::Cav2_YSmallerThanX_Invalid,
    Cap2024_11_EdgeCase::Cav1_TooBig_IndexZero_Cav2_InBounds_Valid,
    Cap2024_11_EdgeCase::Cav1_TooBig_IndexZero_Cav2_ExceedsIdentity_Invalid,
    // Cap2024_11_EdgeCase::Cav1_PushStartOutsideLimit_Invalid,
    // Cap2024_11_EdgeCase::Cav2_PushStartOutsideLimit_Invalid,
    // Cap2024_11_EdgeCase::Cav2_IgnoredBitsOfXYAreSet_Valid,
    Cap2024_11_EdgeCase::Cav2_ShrinksToZero_Valid,
];

fn cap2024_11_length_precision_for_pow2(pow2: u8) -> u8 {
    if pow2 < 10 {
        panic!("no length precision for pow2 {pow2} < 10");
    } else if pow2 <= 15 {
        10
    } else if pow2 <= 22 {
        pow2 - 6
    } else if pow2 <= 29 {
        17
    } else if pow2 <= 36 {
        pow2 - 13
    } else if pow2 <= 43 {
        24
    } else if pow2 <= 50 {
        pow2 - 20
    } else if pow2 <= 57 {
        31
    } else if pow2 <= 64 {
        pow2 - 27
    } else {
        panic!("no length precision for pow2 {pow2} > 64");
    }
}

fn cap2024_11_base_precision_for_len_pow2(pow2: u8) -> u8 {
    if pow2 < 10 {
        panic!("no base precision for pow2 {pow2} < 10");
    } else if pow2 <= 22 {
        10
    } else if pow2 <= 36 {
        17
    } else if pow2 <= 50 {
        24
    } else if pow2 <= 64 {
        31
    } else {
        panic!("no base precision for pow2 {pow2} > 64");
    }
}

/// Randomly generate a range suitable for encoding as an initial resource
/// where the top never exits the encodable base region
/// so that any subsequent caveats will never push the start of the region out of encodable range
fn rand_initial_base_len<R: Rng>(rng: &mut R, initial_len_pow2: Option<u8>) -> AddrRange {
    // If we randomly generated the length directly, we would be biased towards large lengths and not test small ones (which would require a long string of 0s at the start).
    // Instead randomly choose a pow2 then sample inside that
    let log_len: u8 = initial_len_pow2.unwrap_or_else(|| ChooseRand::Between(10, 54).sample(rng));
    let len: u64 = if log_len > 54 || log_len < 10 {
        panic!("tried to create a capability >2^54 or <2^10");
    } else if log_len == 54 {
        1 << 54
    } else {
        ChooseRand::Between(1 << log_len, 1 << (log_len+1)).sample(rng)
    };

    const MAX_BASE: u64 = 1 << 54;
    let base: u64 = ChooseRand::Between(0, MAX_BASE - len).sample(rng);

    AddrRange::base_len_64(base, len)
}

/// Randomly generate a range suitable for shrinking an initial resource with cav1
fn rand_cav1_refined_base_len_non_identity<R: Rng>(rng: &mut R, initial: &CapData) -> (u16, u8) {
    // cav1 selects a pow2-length subrange of the initial range.
    // This pow2 is specified relative to the maximum possible pow2 length of the encoded initial range.
    // This will always be at least 15 - elem_width_log2 = 10, log_max_count = 5
    let cav1_rel_to = initial.initial.elem_width_log2_minus_10.val() + 10 + initial.initial.log_max_count.val();
    // The pow2 we select must also be smaller than the actual pow2 of the allocation inside the initial range
    let initial_range_len = initial.initial.to_start_length().1.val();
    // This may be smaller than cav1_rel_to, e.g. the smallest initial resource is 2^10 while the given cav1_rel_to for that range is 2^15.
    // It will never be larger.
    let smallest_pow2_inside_initial_range = initial_range_len.ilog2();
    assert!(smallest_pow2_inside_initial_range as u8 <= cav1_rel_to);
    let cav1_pow2: u8 = ChooseRand::Between(cav1_rel_to - 15, smallest_pow2_inside_initial_range as u8).sample(rng);

    let cav1_index: u16 = ChooseRand::Between(0, (initial_range_len >> cav1_pow2) as u16 - 1).sample(rng);

    (cav1_index, cav1_pow2)
}

/// Randomly generate a range suitable for shrinking an initial resource with cav1
/// Will never generate a 1-byte range
fn rand_cav1_refined_base_len_non_identity_nonzero_pow2<R: Rng>(rng: &mut R, initial: &CapData) -> (u16, u8) {
    // cav1 selects a pow2-length subrange of the initial range.
    // This pow2 is specified relative to the maximum possible pow2 length of the encoded initial range.
    // This will always be at least 15 - elem_width_log2 = 10, log_max_count = 5
    let cav1_rel_to = initial.initial.elem_width_log2_minus_10.val() + 10 + initial.initial.log_max_count.val();
    // The pow2 we select must also be smaller than the actual pow2 of the allocation inside the initial range
    let initial_range_len = initial.initial.to_start_length().1.val();
    // This may be smaller than cav1_rel_to, e.g. the smallest initial resource is 2^10 while the given cav1_rel_to for that range is 2^15.
    // It will never be larger.
    let smallest_pow2_inside_initial_range = initial_range_len.ilog2();
    assert!(smallest_pow2_inside_initial_range as u8 <= cav1_rel_to);
    let cav1_pow2: u8 = ChooseRand::Between(core::cmp::max(1, cav1_rel_to - 15), smallest_pow2_inside_initial_range as u8).sample(rng);

    let cav1_index: u16 = ChooseRand::Between(0, (initial_range_len >> cav1_pow2) as u16 - 1).sample(rng);

    (cav1_index, cav1_pow2)
}

/// Randomly generate a range suitable for shrinking a cav2 resource with cav1.
fn rand_cav2_refined_base_len_from_nonidentity_cav1<R: Rng>(rng: &mut R, cav1_pow2: u8) -> (u16, u16) {
    const MAX_CAV2_CUTDOWN: u8 = 14;
    match cav1_pow2.checked_sub(MAX_CAV2_CUTDOWN) {
        Some(_) => {
            // the cav1 pow2 >= MAX_CAV2_CUTDOWN so all bits of range_x, range_y will be used
            const MAX_RANGE: u16 = 1 << MAX_CAV2_CUTDOWN;
            // Make x at least 1 so there can always be a smaller y
            let range_x = ChooseRand::Between(0, MAX_RANGE - 1).sample(rng);
            let range_y_minus_one = ChooseRand::Between(range_x, MAX_RANGE - 1).sample(rng);

            (range_x, range_y_minus_one)
        }
        None => {
            // We want elem_count >> (MAX_CAV2_CUTDOWN - cav1_pow2) > 0
            // where elem_count = (range_y_minus_one - range_x) + 1
            // and we want range_x to be such that (range_x >> (MAX_CAV2_CUTDOWN - cav1_pow2)) + elem_count < initial_length
            // and that (range_x >> (MAX_CAV2_CUTDOWN - cav1_pow2)) + 1 <= initial_length
            let split_idx = MAX_CAV2_CUTDOWN - cav1_pow2;

            // We can assume the initial range length = cav1_pow2 because cav1_pow2 is not the "identity" version, which could have been larger than the initial length
            let initial_range_len = 1u16 << cav1_pow2;
            let range_x = ChooseRand::Between(0, (core::cmp::min(1 << MAX_CAV2_CUTDOWN, initial_range_len) as u16) - 1).sample(rng);
            // elem_count here is the 16-bit version calculated before shifting, so it follows the same rules as the normal cav2 and cannot be =0.
            // length=0 is only plausible when truncating elem_count.
            // Select such an elem_count that will be zero once shifted. Constraints:
            // - must be greater than or equal to (1 << first_bit_position_visible_after_shifting)
            // - must not push range_y_minus_one out of the cav1 length
            let elem_count = ChooseRand::Between(
                1 << split_idx,
                (1 << MAX_CAV2_CUTDOWN) - range_x,

            ).sample(rng);
            let range_y_minus_one = elem_count + range_x - 1;

            let exp_start_offset = range_x >> (MAX_CAV2_CUTDOWN - cav1_pow2);
            let exp_elem_count = (range_y_minus_one - range_x + 1) >> (MAX_CAV2_CUTDOWN - cav1_pow2);
            let exp_length = exp_start_offset + exp_elem_count;
            assert!(exp_length <= initial_range_len);

            (range_x, range_y_minus_one)
        }
    }
}

impl<TSignaturer: Signaturer> RandCap for Cap2024_11<TSignaturer> {
    // TODO test with big lengths
    const INITIAL_LEN_POW2_RANGE: core::ops::RangeInclusive<u8> = 10..=54;
    
    fn rand_valid_cap_with_perms_chain<R: Rng>(rng: &mut R, secret: Option<u128>, secret_key_id: Option<u32>, initial_len_pow2: Option<u8>, chain: CapPermsChain) -> CapTestCase {
        let secret = secret.unwrap_or_else(|| ChooseRand::Any.sample(rng));
        let secret_key_id = secret_key_id.unwrap_or_else(|| ChooseRand::Between(0, (1 << 23) - 1).sample(rng));
        // Always start with a 0-cav capability
        let mut cap = {
            let range = rand_initial_base_len(rng, initial_len_pow2);
            Self::new_inexact(secret, secret_key_id, range.base(), range.len(), chain.root().into()).unwrap()
        };

        // If a cav1 was requested, generate one
        if let Some(chain_cav1) = chain.at_cav_1() {
            let (index, pow2) = rand_cav1_refined_base_len_non_identity(rng, &cap.cap_data);
            cap = cap.with_cav1(Some(chain_cav1.into()), index, pow2).unwrap();

            // If a cav2 was requested, generate one
            if let Some(chain_cav2) = chain.at_cav_2() {
                let (range_x, range_y_minus_one) = rand_cav2_refined_base_len_from_nonidentity_cav1(rng, pow2);
                cap = cap.with_cav2(Some(chain_cav2.into()), range_x, range_y_minus_one + 1).unwrap();
                assert!(cap.addr_range.len() > 0, "{:?}", cap);
            }
        }

        let expected = cap.valid_range(secret);
        assert!(expected.is_ok());

        CapTestCase {
            secret,
            sig: cap.sig,
            data: cap.data,
            expected,
        }
    }

    type EdgeCase = Cap2024_11_EdgeCase;

    fn enum_edge_cases() -> &'static [Self::EdgeCase] {
        &CAP2024_11_EDGE_CASES
    }

    // TODO test with big lengths

    fn rand_edge_case<R: Rng>(rng: &mut R, secret: Option<u128>, secret_key_id: Option<u32>, edge_case: Self::EdgeCase) -> CapTestCase {
        let secret = secret.unwrap_or_else(|| ChooseRand::Any.sample(rng));
        let secret_key_id = secret_key_id.unwrap_or_else(|| ChooseRand::Between(0, (1 << 23) - 1).sample(rng));

        match edge_case {
            // Cap2024_11_EdgeCase::All_Signature_OffByOne => todo!(),
            // Cap2024_11_EdgeCase::All_Signature_Outdated => todo!(),
            Cap2024_11_EdgeCase::Cav1_NonZeroWhenUnused => {
                let chain = choose_from(rng, &ALL_0CAV_CAP_PERMS_CHAINS);
                // Start with a 0-cav capability
                let cap = {
                    let range = rand_initial_base_len(rng, None);
                    Self::new_inexact(secret, secret_key_id, range.base(), range.len(), chain.root().into()).unwrap()
                };

                let sig = cap.sig;
                let data = {
                    let mut cap_bitfield = CapBitfield::new(cap.data);
                    cap_bitfield.set_index(ChooseRand::Between(1u16, (1 << 15) - 1).sample(rng) as u128);
                    cap_bitfield.set_index_size_div(ChooseRand::Any.sample(rng));
                    cap_bitfield.val
                };

                assert_eq!(Self::from_bits(sig, data), Err(CapDecodeErr::UnexpectedCaveat));
                
                CapTestCase {
                    secret,
                    sig,
                    data,
                    expected: Err(CapDecodeErr::UnexpectedCaveat),
                }
            },
            Cap2024_11_EdgeCase::Cav1_TooBig_IndexNonZero_Invalid => {
                // Select a random capability which uses cav1
                let chain = choose_from(rng, &ALL_1CAV_CAP_PERMS_CHAINS);
                assert!(chain.at_cav_1().is_some());
                assert_eq!(chain.at_cav_2(), None);

                // We need a random initial capability where 1) the length is reasonably uniformly log distributed
                // and 2) the length is not an exact power of two.
                // We cannot guarantee 2) if we use Self::new_inexact because that may round things based on the base and length.
                // Therefore we construct the capability directly from bits, randomly selecting the encoded-elem-width from a uniform distribution (and thus satisfying point 1)
                // and randomly generating the b|c field. The bottom bit of the b|c field is zeroed to satisfy point 2 - a pow2 length requires a count = 0b01000...00, and count is encoded with an implicit +1
                // i.e. if c = 1_1111_1111 count = (c+1) = 10_0000_0000
                // therefore if c does not have the bottom bit set it cannot encode a pow2 length.
                // NOTE: THIS RISKS GENERATING SUBNORMAL CAPABILITIES!
                let cap ={
                    let mut cap_bitfield = CapBitfield::new(0);
                    cap_bitfield.set_b_c(ChooseRand::Between(0, (1 << 49) - 1).sample(rng) & (!1)); 
                    cap_bitfield.set_encoded_elem_width(ChooseRand::Between(0, 31).sample(rng));
                    cap_bitfield.set_perms(chain.root().encode() as u128);
                    cap_bitfield.set_secret_key_id(secret_key_id as u128);
                    let data = cap_bitfield.val;
                    let sig = TSignaturer::sign(&u128_to_byte_array(data), secret);
                    
                    Self::from_bits(sig, data).unwrap()
                };

                let cav1_rel_to = cap.cap_data.initial.elem_width_log2_minus_10.val() + 10 + cap.cap_data.initial.log_max_count.val();
                // The pow2 we select must be *larger* than the allocation inside the initial range
                // This may not be exactly equal to `len`
                let initial_range_len = cap.cap_data.initial.to_start_length().1.val();
                // This may be smaller than cav1_rel_to, e.g. the smallest initial resource is 2^10 while the given cav1_rel_to for that range is 2^15.
                // It will never be larger.
                let smallest_pow2_inside_initial_range = initial_range_len.ilog2() as u8;
                assert!(smallest_pow2_inside_initial_range < cav1_rel_to);
                // In case the initial capability is subnormal
                // (i.e. the initial range doesn't implicitly set the top bit of the count bitfield, representing a much smaller range than expected given the elem_width)
                // the length of the initial range may be less than 2^(cav1_rel_to - 15).
                // Clamp the "smallest cav1 pow2 outside initial range" to a pow2 that cav1 can actually represent.
                let smallest_cav1_pow2_outside_initial_range = core::cmp::max(smallest_pow2_inside_initial_range + 1, cav1_rel_to.saturating_sub(15));
                let cav1_pow2 = ChooseRand::Between(smallest_cav1_pow2_outside_initial_range, cav1_rel_to).sample(rng);

                // It should always be possible to use these pow2s as identity caveats when index=0...
                for pow2 in smallest_cav1_pow2_outside_initial_range..=cav1_rel_to {
                    assert!(cap.with_cav1(None, 0, pow2).is_ok());
                }

                // ...but with a nonzero index it should all fall apart
                let cav1_index = ChooseRand::Between(1, (1 << 15) - 1).sample(rng);
                assert_eq!(cap.with_cav1(None, cav1_index, cav1_pow2), Err(CapEncodeErr::InvalidCaveat));

                let sig = cap.sig;
                let data = {
                    let mut cap_bitfield = CapBitfield::new(cap.data);
                    cap_bitfield.set_index(cav1_index as u128);
                    cap_bitfield.set_index_size_div((cav1_rel_to - cav1_pow2) as u128);
                    cap_bitfield.set_perms(chain.encode() as u128);
                    cap_bitfield.val
                };

                assert_eq!(Self::from_bits(sig, data), Err(CapDecodeErr::InvalidCaveat));
                
                CapTestCase {
                    secret,
                    sig,
                    data,
                    expected: Err(CapDecodeErr::InvalidCaveat),
                }
            },
            Cap2024_11_EdgeCase::Cav1_TooBig_IndexZero_Valid => {
                // Select a random capability which uses cav1
                let chain = choose_from(rng, &ALL_1CAV_CAP_PERMS_CHAINS);
                assert!(chain.at_cav_1().is_some());
                assert_eq!(chain.at_cav_2(), None);

                // We need a random initial capability where 1) the length is reasonably uniformly log distributed
                // and 2) the length is not an exact power of two.
                // We cannot guarantee 2) if we use Self::new_inexact because that may round things based on the base and length.
                // Therefore we construct the capability directly from bits, randomly selecting the encoded-elem-width from a uniform distribution (and thus satisfying point 1)
                // and randomly generating the b|c field. The bottom bit of the b|c field is zeroed to satisfy point 2 - a pow2 length requires a count = 0b01000...00, and count is encoded with an implicit +1
                // i.e. if c = 1_1111_1111 count = (c+1) = 10_0000_0000
                // therefore if c does not have the bottom bit set it cannot encode a pow2 length.
                // NOTE: THIS RISKS GENERATING SUBNORMAL CAPABILITIES!
                let cap ={
                    let mut cap_bitfield = CapBitfield::new(0);
                    cap_bitfield.set_b_c(ChooseRand::Between(0, (1 << 49) - 1).sample(rng) & (!1)); 
                    cap_bitfield.set_encoded_elem_width(ChooseRand::Between(0, 31).sample(rng));
                    cap_bitfield.set_perms(chain.root().encode() as u128);
                    cap_bitfield.set_secret_key_id(secret_key_id as u128);
                    let data = cap_bitfield.val;
                    let sig = TSignaturer::sign(&u128_to_byte_array(data), secret);
                    
                    Self::from_bits(sig, data).unwrap()
                };

                let cav1_rel_to = cap.cap_data.initial.elem_width_log2_minus_10.val() + 10 + cap.cap_data.initial.log_max_count.val();
                // The pow2 we select must be *larger* than the allocation inside the initial range
                // This may not be exactly equal to `len`
                let initial_range_len = cap.cap_data.initial.to_start_length().1.val();
                // This may be smaller than cav1_rel_to, e.g. the smallest initial resource is 2^10 while the given cav1_rel_to for that range is 2^15.
                // It will never be larger.
                let smallest_pow2_inside_initial_range = initial_range_len.ilog2() as u8;
                assert!(smallest_pow2_inside_initial_range < cav1_rel_to);
                // In case the initial capability is subnormal
                // (i.e. the initial range doesn't implicitly set the top bit of the count bitfield, representing a much smaller range than expected given the elem_width)
                // the length of the initial range may be less than 2^(cav1_rel_to - 15).
                // Clamp the "smallest cav1 pow2 outside initial range" to a pow2 that cav1 can actually represent.
                let smallest_cav1_pow2_outside_initial_range = core::cmp::max(smallest_pow2_inside_initial_range + 1, cav1_rel_to.saturating_sub(15));
                let cav1_pow2 = ChooseRand::Between(smallest_cav1_pow2_outside_initial_range, cav1_rel_to).sample(rng);

                // It should always be possible to use these pow2s as identity caveats when index=0...
                for pow2 in smallest_cav1_pow2_outside_initial_range..=cav1_rel_to {
                    assert!(cap.with_cav1(None, 0, pow2).is_ok());
                }

                let cap = cap.with_cav1(Some(chain.at_cav_1().unwrap().into()), 0, cav1_pow2).unwrap();
                let expected = cap.valid_range(secret);
                assert!(expected.is_ok());

                CapTestCase {
                    secret,
                    sig: cap.sig,
                    data: cap.data,
                    expected,
                }
            },
            Cap2024_11_EdgeCase::Cav2_NonZeroWhenUnused_Invalid => {
                // Select a random capability which may or may not use cav1
                let chain = choose_from(rng, &ALL_0_OR_1CAV_CAP_PERMS_CHAINS);
                // Start with a 0-cav capability
                let mut cap = {
                    let range = rand_initial_base_len(rng, None);
                    Self::new_inexact(secret, secret_key_id, range.base(), range.len(), chain.root().into()).unwrap()
                };
                // If a cav1 was requested, generate one
                if let Some(chain_cav1) = chain.at_cav_1() {
                    let (index, pow2) = rand_cav1_refined_base_len_non_identity(rng, &cap.cap_data);
                    cap = cap.with_cav1(Some(chain_cav1.into()), index, pow2).unwrap();
                }
                assert_eq!(chain.at_cav_2(), None);

                let sig = cap.sig;
                let data = {
                    let range_x: u16 = ChooseRand::Any.sample(rng);
                    let range_y_minus_one: u16 = ChooseRand::Any.sample(rng);

                    let mut cap_bitfield = CapBitfield::new(cap.data);
                    cap_bitfield.set_range_x(range_x as u128);
                    cap_bitfield.set_range_y_minus_one(range_y_minus_one as u128);
                    cap_bitfield.val
                };

                assert_eq!(Self::from_bits(sig, data), Err(CapDecodeErr::UnexpectedCaveat));
                
                CapTestCase {
                    secret,
                    sig,
                    data,
                    expected: Err(CapDecodeErr::UnexpectedCaveat),
                }
            },
            Cap2024_11_EdgeCase::Cav2_YSmallerThanX_Invalid => {
                // Select a random capability which uses cav1 and cav2
                let chain = choose_from(rng, &ALL_2CAV_CAP_PERMS_CHAINS);
                // Start with a 0-cav capability
                let mut cap = {
                    let range = rand_initial_base_len(rng, None);
                    Self::new_inexact(secret, secret_key_id, range.base(), range.len(), chain.root().into()).unwrap()
                };
                assert!(chain.at_cav_1().is_some());
                assert!(chain.at_cav_2().is_some());
                // cav1 was requested, generate one where the pow2 will never be 0
                let (index, pow2) = rand_cav1_refined_base_len_non_identity_nonzero_pow2(rng, &cap.cap_data);
                cap = cap.with_cav1(Some(chain.at_cav_1().unwrap().into()), index, pow2).unwrap();

                // cav2 was requested, generate one
                let sig = cap.sig;
                const MAX_CAV2_CUTDOWN: u8 = 14;
                let data = {
                    let (range_x, range_y_minus_one) = match pow2.checked_sub(MAX_CAV2_CUTDOWN) {
                        Some(_) => {
                            // the cav1 pow2 >= MAX_CAV2_CUTDOWN so all bits of range_x, range_y will be used
                            const MAX_RANGE: u16 = 1 << MAX_CAV2_CUTDOWN;
                            // Make x at least 1 so there can always be a smaller y
                            let range_x = ChooseRand::Between(1, MAX_RANGE - 1).sample(rng);
                            let range_y_minus_one = ChooseRand::Between(0, range_x - 1).sample(rng);

                            (range_x, range_y_minus_one)
                        }
                        None => {
                            // the cav1 pow2 < MAX_CAV2_CUTDOWN therefore the leftmost (pow2) bits will be used
                            let max_range: u16 = 1 << pow2;
                            // Make x at least 1 so there can always be a smaller y
                            let range_x = ChooseRand::Between(1, max_range - 1).sample(rng);
                            let range_y_minus_one = ChooseRand::Between(0, range_x - 1).sample(rng);

                            // Make range_x and range_y left aligned
                            (range_x << (MAX_CAV2_CUTDOWN - pow2), range_y_minus_one << (MAX_CAV2_CUTDOWN - pow2))
                        }
                    };


                    let mut cap_bitfield = CapBitfield::new(cap.data);
                    cap_bitfield.set_range_x(range_x as u128);
                    cap_bitfield.set_range_y_minus_one(range_y_minus_one as u128);
                    cap_bitfield.set_perms(chain.encode() as u128);
                    cap_bitfield.val
                };

                
                assert_eq!(Self::from_bits(sig, data), Err(CapDecodeErr::InvalidCaveat));
                
                CapTestCase {
                    secret,
                    sig,
                    data,
                    expected: Err(CapDecodeErr::InvalidCaveat),
                }
            },
            Cap2024_11_EdgeCase::Cav1_TooBig_IndexZero_Cav2_InBounds_Valid => {
                // Select a random capability which uses cav1 and cav2
                let chain = choose_from(rng, &ALL_2CAV_CAP_PERMS_CHAINS);
                assert!(chain.at_cav_1().is_some());
                assert!(chain.at_cav_2().is_some());

                // We need a random initial capability where 1) the length is reasonably uniformly log distributed
                // and 2) the length is not an exact power of two.
                // We cannot guarantee 2) if we use Self::new_inexact because that may round things based on the base and length.
                // Therefore we construct the capability directly from bits, randomly selecting the encoded-elem-width from a uniform distribution (and thus satisfying point 1)
                // and randomly generating the b|c field. The bottom bit of the b|c field is zeroed to satisfy point 2 - a pow2 length requires a count = 0b01000...00, and count is encoded with an implicit +1
                // i.e. if c = 1_1111_1111 count = (c+1) = 10_0000_0000
                // therefore if c does not have the bottom bit set it cannot encode a pow2 length.
                // NOTE: THIS RISKS GENERATING SUBNORMAL CAPABILITIES!
                // Subnormal capabilities CANNOT always generate in-bounds cav2.
                // There are three possible subnormal capabilities:
                // - where elem_width_log_2 = 17, elem_count_bits = 12, max length = 2^29
                // - where elem_width_log_2 = 24, elem_count_bits = 19, max length = 2^43
                // - where elem_width_log_2 = 31, elem_count_bits = 26, max length = 2^57
                // count is always >= 1, therefore the min length = 2^elem_width_log_2
                // cav1_rel_to = elem_width_log_2 + elem_count_bits
                // therefore cav1_rel_to and cav1_pow2 is at most 26 more than elem_width_log_2
                // therefore cav2_pow2 is at most 12 more than elem_width_log_2... ah shite.
                // therefore the cav2 granularity (2^cav2_pow2) is at most 2^12 times the minimum length (2^elem_width_log_2).
                // in practice this just seems very unlikely? like the chances of selecting a specific elem_width_log_2 and a very low count portion... but I don't like it.
                // solution: select cav1_pow2 such that in the subnormal case it is at most 14 more than the smallest_pow2_inside_inital_range
                let cap ={
                    let mut cap_bitfield = CapBitfield::new(0);
                    cap_bitfield.set_b_c(ChooseRand::Between(0, (1 << 49) - 1).sample(rng) & (!1)); 
                    cap_bitfield.set_encoded_elem_width(ChooseRand::Between(0, 31).sample(rng));
                    cap_bitfield.set_perms(chain.root().encode() as u128);
                    cap_bitfield.set_secret_key_id(secret_key_id as u128);
                    let data = cap_bitfield.val;
                    let sig = TSignaturer::sign(&u128_to_byte_array(data), secret);
                    
                    Self::from_bits(sig, data).unwrap()
                };

                let cav1_rel_to = cap.cap_data.initial.elem_width_log2_minus_10.val() + 10 + cap.cap_data.initial.log_max_count.val();
                // The pow2 we select must be *larger* than the allocation inside the initial range
                // This may not be exactly equal to `len`
                let initial_range_len = cap.cap_data.initial.to_start_length().1.val();
                // This may be smaller than cav1_rel_to, e.g. the smallest initial resource is 2^10 while the given cav1_rel_to for that range is 2^15.
                // It will never be larger.
                let smallest_pow2_inside_initial_range = initial_range_len.ilog2() as u8; // will always be >=10
                assert!(smallest_pow2_inside_initial_range < cav1_rel_to);
                // In case the initial capability is subnormal
                // (i.e. the initial range doesn't implicitly set the top bit of the count bitfield, representing a much smaller range than expected given the elem_width)
                // the length of the initial range may be less than 2^(cav1_rel_to - 15).
                // Clamp the "smallest cav1 pow2 outside initial range" to a pow2 that cav1 can actually represent.
                let smallest_cav1_pow2_outside_initial_range = core::cmp::max(smallest_pow2_inside_initial_range + 1, cav1_rel_to.saturating_sub(15));
                // In case the inital capability is subnormal, select a maximum cav1 such that the cav2_pow2 is <= smallest_pow2_inside_initial_range
                // smallest_pow2_inside_initial_range is always accessible from cav1_rel_to in each of the three subnormal cases.
                let largest_cav1_pow2_that_allows_inbounds_cav2 = core::cmp::min(cav1_rel_to, smallest_pow2_inside_initial_range + 14);
                let cav1_pow2 = ChooseRand::Between(smallest_cav1_pow2_outside_initial_range, largest_cav1_pow2_that_allows_inbounds_cav2).sample(rng);

                // It should always be possible to use these pow2s as identity caveats when index=0...
                for pow2 in smallest_cav1_pow2_outside_initial_range..=cav1_rel_to {
                    assert!(cap.with_cav1(None, 0, pow2).is_ok());
                }

                let cap = cap.with_cav1(Some(chain.at_cav_1().unwrap().into()), 0, cav1_pow2).unwrap();

                const MAX_CAV2_CUTDOWN: u8 = 14;
                let (range_x, range_y_minus_one) = {
                    // range values are relative to cav1_pow2 - 14
                    // cav1_pow2 may not be equal to cav0 pow2 + cav0 logmaxcount
                    // 
                    // use initial_range_len >> 
                    match cav1_pow2.checked_sub(MAX_CAV2_CUTDOWN) {
                        Some(_) => {
                            // the cav1 pow2 >= MAX_CAV2_CUTDOWN so all bits of range_x, range_y will be used
                            const _MAX_RANGE: u16 = 1 << MAX_CAV2_CUTDOWN;
                            // TODO check bit indexes
                            let mut first_range_val_outside_cav1 = bits!(initial_range_len, (cav1_pow2 as usize - 14):(cav1_pow2 as usize - 1)) as u16;
                            if cav1_pow2 > 14 && bits!(initial_range_len, 0:(cav1_pow2 as usize - 15)) > 0 {
                                first_range_val_outside_cav1 += 1;
                            }

                            assert!(first_range_val_outside_cav1 >= 1);

                            let range_x = ChooseRand::Between(0, first_range_val_outside_cav1.saturating_sub(3)).sample(rng);
                            let range_y_minus_one = ChooseRand::Between(range_x, first_range_val_outside_cav1.saturating_sub(2)).sample(rng);

                            (range_x, range_y_minus_one)
                        }
                        None => {
                            // the cav1 pow2 < MAX_CAV2_CUTDOWN therefore the leftmost (pow2) bits will be used
                            let _max_range: u16 = 1 << cav1_pow2;
                            // TODO check bit indexes
                            let first_range_val_outside_cav1 = bits!(initial_range_len, 0:(cav1_pow2 as usize - 1)) as u16;

                            assert!(first_range_val_outside_cav1 >= 1);

                            let range_x = ChooseRand::Between(0, first_range_val_outside_cav1.saturating_sub(3)).sample(rng);
                            let range_y_minus_one = ChooseRand::Between(range_x, first_range_val_outside_cav1.saturating_sub(2)).sample(rng);

                            // Make range_x and range_y left aligned
                            (range_x << (MAX_CAV2_CUTDOWN - cav1_pow2), range_y_minus_one << (MAX_CAV2_CUTDOWN - cav1_pow2))
                        }
                    }
                };
                let cap = cap.with_cav2(Some(chain.at_cav_2().unwrap().into()), range_x, range_y_minus_one + 1).unwrap();

                let expected = cap.valid_range(secret);
                assert!(expected.is_ok());

                CapTestCase {
                    secret,
                    sig: cap.sig,
                    data: cap.data,
                    expected,
                }
            },
            Cap2024_11_EdgeCase::Cav1_TooBig_IndexZero_Cav2_ExceedsIdentity_Invalid => {
                
                // Select a random capability which uses cav1 and cav2
                let chain = choose_from(rng, &ALL_2CAV_CAP_PERMS_CHAINS);
                assert!(chain.at_cav_1().is_some());
                assert!(chain.at_cav_2().is_some());

                // We need a random initial capability where 1) the length is reasonably uniformly log distributed
                // and 2) the length is not an exact power of two.
                // We cannot guarantee 2) if we use Self::new_inexact because that may round things based on the base and length.
                // Therefore we construct the capability directly from bits, randomly selecting the encoded-elem-width from a uniform distribution (and thus satisfying point 1)
                // and randomly generating the b|c field. The bottom bit of the b|c field is zeroed to satisfy point 2 - a pow2 length requires a count = 0b01000...00, and count is encoded with an implicit +1
                // i.e. if c = 1_1111_1111 count = (c+1) = 10_0000_0000
                // therefore if c does not have the bottom bit set it cannot encode a pow2 length.
                // NOTE: THIS RISKS GENERATING SUBNORMAL CAPABILITIES!
                // Subnormal capabilities may make in-bounds cav2 impossible, but that's fine - we specifically want out-of-bounds cav2
                let cap ={
                    let mut cap_bitfield = CapBitfield::new(0);
                    cap_bitfield.set_b_c(ChooseRand::Between(0, (1 << 49) - 1).sample(rng) & (!1)); 
                    cap_bitfield.set_encoded_elem_width(ChooseRand::Between(0, 31).sample(rng));
                    cap_bitfield.set_perms(chain.root().encode() as u128);
                    cap_bitfield.set_secret_key_id(secret_key_id as u128);
                    let data = cap_bitfield.val;
                    let sig = TSignaturer::sign(&u128_to_byte_array(data), secret);
                    
                    Self::from_bits(sig, data).unwrap()
                };

                let cav1_rel_to = cap.cap_data.initial.elem_width_log2_minus_10.val() + 10 + cap.cap_data.initial.log_max_count.val();
                // The pow2 we select must be *larger* than the allocation inside the initial range
                // This may not be exactly equal to `len`
                let initial_range_len = cap.cap_data.initial.to_start_length().1.val();
                // This may be smaller than cav1_rel_to, e.g. the smallest initial resource is 2^10 while the given cav1_rel_to for that range is 2^15.
                // It will never be larger.
                let smallest_pow2_inside_initial_range = initial_range_len.ilog2() as u8;
                assert!(smallest_pow2_inside_initial_range < cav1_rel_to);
                // In case the initial capability is subnormal
                // (i.e. the initial range doesn't implicitly set the top bit of the count bitfield, representing a much smaller range than expected given the elem_width)
                // the length of the initial range may be less than 2^(cav1_rel_to - 15).
                // Clamp the "smallest cav1 pow2 outside initial range" to a pow2 that cav1 can actually represent.
                let smallest_cav1_pow2_outside_initial_range = core::cmp::max(smallest_pow2_inside_initial_range + 1, cav1_rel_to.saturating_sub(15));
                let cav1_pow2 = ChooseRand::Between(smallest_cav1_pow2_outside_initial_range, cav1_rel_to).sample(rng);

                // It should always be possible to use these pow2s as identity caveats when index=0...
                for pow2 in smallest_cav1_pow2_outside_initial_range..=cav1_rel_to {
                    assert!(cap.with_cav1(None, 0, pow2).is_ok());
                }

                let cap = cap.with_cav1(Some(chain.at_cav_1().unwrap().into()), 0, cav1_pow2).unwrap();

                // cav2 was requested, generate one
                let sig = cap.sig;
                const MAX_CAV2_CUTDOWN: u8 = 14;
                let data = {
                    let (range_x, range_y_minus_one): (u16, u16) = match cav1_pow2.checked_sub(MAX_CAV2_CUTDOWN) {
                        Some(_) => {
                            // the cav1 pow2 >= MAX_CAV2_CUTDOWN so all bits of range_x, range_y will be used
                            let mut first_range_val_outside_cav1 = bits!(initial_range_len, (cav1_pow2 as usize - 14):(cav1_pow2 as usize - 1)) as u16;
                            if cav1_pow2 > 14 && bits!(initial_range_len, 0:(cav1_pow2 as usize - 15)) > 0 {
                                first_range_val_outside_cav1 += 1;
                            }

                            const MAX_RANGE: u16 = 1 << MAX_CAV2_CUTDOWN;
                            // if the number of count bits in the initial resource is >14, it's possible for the top 14 bits of the length to all be set and for there to be nonzero values in the lower bits.
                            // this means first_range_val_outside_cav1 = (1 << 14).
                            // in this case, override range_y_minus_one = MAX_RANGE - 1, which is the only way to ensure the cav2 == cav1 i.e. cav2 exceeds the initial resource.
                            // we use first_range_val_outside_cav1 to set the minimum value of range_y_minus_one, which is why we set it here instead of setting range_y_minus_one directly.
                            first_range_val_outside_cav1 = core::cmp::min(first_range_val_outside_cav1, MAX_RANGE - 1);

                            assert!(first_range_val_outside_cav1 < MAX_RANGE);

                            let range_x = ChooseRand::Between(0, MAX_RANGE - 2).sample(rng);
                            let range_y_minus_one = ChooseRand::Between(core::cmp::max(range_x, first_range_val_outside_cav1), MAX_RANGE - 1).sample(rng);

                            (range_x, range_y_minus_one)
                        }
                        None => {
                            // cav1_pow2 is 13 or lower, which is larger than the initial_range_len, therefore first_range_val_outside_cav1 < (1 << 13)
                            let first_range_val_outside_cav1 = initial_range_len as u16;
                            
                            // the cav1 pow2 < MAX_CAV2_CUTDOWN therefore the leftmost (pow2) bits will be used
                            let max_range: u16 = 1 << cav1_pow2;
                            let range_x = ChooseRand::Between(0, max_range - 2).sample(rng);
                            // We have to set range_y_minus_one to be at minimum first_range_val_outside_cav1 + 1, unlike in the normal setting,
                            // because here the resulting elem_count = ((range_y_minus_one << BLAH) - (range_x << BLAH) + 1) >> BLAH - the plus-one that makes this unnecessary above isn't present here.
                            // first_range_val_outside_cav1 < (1 << cav1_pow2), and first_range_val_outside_cav1[9:0] = 0
                            // => first_range_val_outside_cav1 + 1 <= (1 << cav1_pow2) - 1
                            // => first_range_val_outside_cav1 + 1 <= max_range - 1
                            let range_y_minus_one = ChooseRand::Between(core::cmp::max(range_x, first_range_val_outside_cav1 + 1), max_range - 1).sample(rng);

                            // Shift them up so they're both 14-bit
                            let range_x = range_x << (MAX_CAV2_CUTDOWN - cav1_pow2);
                            let range_y_minus_one = range_y_minus_one << (MAX_CAV2_CUTDOWN - cav1_pow2);

                            let exp_start_offset = range_x >> (MAX_CAV2_CUTDOWN - cav1_pow2);
                            let exp_elem_count = (range_y_minus_one - range_x + 1) >> (MAX_CAV2_CUTDOWN - cav1_pow2);
                            let exp_length = exp_start_offset + exp_elem_count;
                            assert!(exp_length > initial_range_len as u16);

                            // Make range_x and range_y left aligned
                            (range_x, range_y_minus_one)
                        }
                    };

                    let mut cap_bitfield = CapBitfield::new(cap.data);
                    cap_bitfield.set_range_x(range_x as u128);
                    cap_bitfield.set_range_y_minus_one(range_y_minus_one as u128);
                    cap_bitfield.set_perms(chain.encode() as u128);
                    cap_bitfield.val
                };

                
                assert_eq!(Self::from_bits(sig, data), Err(CapDecodeErr::InvalidCaveat));
                
                CapTestCase {
                    secret,
                    sig,
                    data,
                    expected: Err(CapDecodeErr::InvalidCaveat),
                }
            },
            // Cap2024_11_EdgeCase::Cav1_PushStartOutsideLimit_Invalid => todo!(),
            // Cap2024_11_EdgeCase::Cav2_PushStartOutsideLimit_Invalid => todo!(),
            // Cap2024_11_EdgeCase::Cav2_IgnoredBitsOfXYAreSet_Valid => todo!(),
            Cap2024_11_EdgeCase::Cav2_ShrinksToZero_Valid => {
                // Generate a capability where the choice of range_x and range_y in cav2 shrinks the range down to zero.
                // This is only possible when using a cav1 with a pow2 less than the cav2 shrink (i.e. 14 bits for cap2024_11)
                // therefore the initial capability can have a maximum length of 2^(cav1_shrink + cav2_shrink - 1) = 2^(15 + 14 - 1).
                // In practice this means the maximum length is 1 << 22, because anything larger than that enters the no-implicit-1 block where cav1 is relative to 29
                let chain = choose_from(rng, &ALL_2CAV_CAP_PERMS_CHAINS);
                assert!(chain.at_cav_1().is_some());
                assert!(chain.at_cav_2().is_some());

                const MAX_CAV1_CUTDOWN: u8 = 15;
                const MAX_CAV2_CUTDOWN: u8 = 14;

                let cap = {
                    // Always select an in
                    let initial_len_pow2 = ChooseRand::Between(10, 21).sample(rng);
                    let range = rand_initial_base_len(rng, Some(initial_len_pow2));
                    Self::new_inexact(secret, secret_key_id, /* range.base() */0, range.len(), chain.root().into()).unwrap()
                };
                let initial_range_len = cap.addr_range.len() as u64;
                assert!(initial_range_len <= (1 << (MAX_CAV1_CUTDOWN + MAX_CAV2_CUTDOWN - 1)));

                // Select a cav1 which is 
                // 1. less than MAX_CAV2_CUTDOWN - 1
                // 2. accessible from the initial length (i.e. if the initial length is 2^27 we can only reach 2^12)

                // At most cav1_rel_to = MAX_CAV1_CUTDOWN + MAX_CAV2_CUTDOWN - 1, so subtracting MAX_CAV1_CUTDOWN will always result in a number <= MAX_CAV2_CUTDOWN
                let cav1_rel_to = cap.cap_data.initial.elem_width_log2_minus_10.val() + 10 + cap.cap_data.initial.log_max_count.val();
                assert!(cav1_rel_to <= (MAX_CAV1_CUTDOWN + MAX_CAV2_CUTDOWN - 1), "{} {}", cav1_rel_to, initial_range_len);
                let cav1_pow2 = ChooseRand::Between(cav1_rel_to.saturating_sub(MAX_CAV1_CUTDOWN), core::cmp::min(cav1_rel_to, MAX_CAV2_CUTDOWN - 1)).sample(rng);
                assert!(cav1_pow2 < MAX_CAV2_CUTDOWN);
                // cav1_rel_to is 15 for any initial resource from 10..=15, so initial_range_len >> cav1_pow2 may be 0
                let cav1_index: u16 = ChooseRand::Between(0, ((initial_range_len >> cav1_pow2) as u16).saturating_sub(1)).sample(rng);
            
                let cap = cap.with_cav1(Some(chain.at_cav_1().unwrap().into()), cav1_index, cav1_pow2).unwrap();

                // We want elem_count >> (MAX_CAV2_CUTDOWN - cav1_pow2) = 0
                // where elem_count = (range_y_minus_one - range_x) + 1
                // and we want range_x to be such that (range_x >> (MAX_CAV2_CUTDOWN - cav1_pow2)) + elem_count < initial_length
                // and that (range_x >> (MAX_CAV2_CUTDOWN - cav1_pow2)) + 1 <= initial_length
                let (range_x, range_y_minus_one) = {                           
                    let split_idx = MAX_CAV2_CUTDOWN - cav1_pow2;

                    let range_x = ChooseRand::Between(0, (core::cmp::min(1 << MAX_CAV2_CUTDOWN, initial_range_len) as u16) - 1).sample(rng);
                    // elem_count here is the 16-bit version calculated before shifting, so it follows the same rules as the normal cav2 and cannot be =0.
                    // length=0 is only plausible when truncating elem_count.
                    // Select such an elem_count that will be zero once shifted. Constraints:
                    // - must be less than (1 << first_bit_position_visible_after_shifting)
                    // - must not push range_y_minus_one out of the cav1 length
                    let elem_count = ChooseRand::Between(1, core::cmp::min(
                        (1 << split_idx) - 1,
                        core::cmp::min(
                            initial_range_len - (range_x >> (MAX_CAV2_CUTDOWN - cav1_pow2)) as u64,
                            (1 << MAX_CAV2_CUTDOWN) - range_x as u64,
                        ) as u16
                    )).sample(rng);
                    let range_y_minus_one = elem_count + range_x - 1;

                    let exp_start_offset = range_x >> (MAX_CAV2_CUTDOWN - cav1_pow2);
                    let exp_elem_count = (range_y_minus_one - range_x + 1) >> (MAX_CAV2_CUTDOWN - cav1_pow2);
                    let exp_length = exp_start_offset + exp_elem_count;
                    assert!((exp_length as u64) < initial_range_len);

                    (range_x, range_y_minus_one)
                };

                assert!(range_x < (1 << 14));
                assert!(range_y_minus_one < (1 << 14), "0x{:x} !< 0x{:x}", range_y_minus_one, 1 << 14);
                assert!(range_y_minus_one >= range_x);

                let cap = cap.with_cav2(Some(chain.into()), range_x, range_y_minus_one + 1).unwrap();

                assert_eq!(cap.addr_range.len(), 0);

                let expected = cap.valid_range(secret);
                assert!(expected.is_ok());

                CapTestCase {
                    secret,
                    sig: cap.sig,
                    data: cap.data,
                    expected,
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{capability::{v2024::rand::RandCap, v2024_11::Cap2024_11}, crypto::NullSig};

    #[test]
    fn generate_lots_of_caps() {
        let mut rng = rand::thread_rng();
        for _ in 0..10_000 {
            Cap2024_11::<NullSig>::rand_cap(&mut rng, None, None, None);
        }
    }

    #[test]
    fn generate_lots_of_edge_caps() {
        let mut rng = rand::thread_rng();
        for edge_case in Cap2024_11::<NullSig>::enum_edge_cases() {
            for _ in 0..10_000 {
                Cap2024_11::<NullSig>::rand_edge_case(&mut rng, None, None, *edge_case);
            }
        }
    }
}