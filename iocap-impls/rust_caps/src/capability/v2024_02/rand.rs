use rand::prelude::*;

use crate::{capability::{v2024::{perms::{CapPermsChain, ALL_0CAV_CAP_PERMS_CHAINS, ALL_0_OR_1CAV_CAP_PERMS_CHAINS, ALL_2CAV_CAP_PERMS_CHAINS}, rand::{choose_from, CapTestCase, ChooseRand, RandCap}, Cap2024}, AddrRange, CapDecodeErr, Capability}, crypto::Signaturer};

use super::{cavs::ToStartLength, Cap2024_02, CapBitfield, CapData};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum Cap2024_02_EdgeCase {
    All_Signature_OffByOne,
    All_Signature_Outdated,
    Cav1_NonZeroWhenUnused,
    Cav1_TooBig_Invalid,
    Cav2_NonZeroWhenUnused_Invalid,
    Cav2_YSmallerThanX_Invalid,
    Cav1_PushStartOutsideLimit_Invalid,
    Cav2_PushStartOutsideLimit_Invalid,
    Cav2_IgnoredBitsOfXYAreSet_Valid,
}
impl From<Cap2024_02_EdgeCase> for &'static core::ffi::CStr {
    fn from(value: Cap2024_02_EdgeCase) -> Self {
        match value {
            Cap2024_02_EdgeCase::All_Signature_OffByOne => c"Cap2024_02_EdgeCase::All_Signature_OffByOne",
            Cap2024_02_EdgeCase::All_Signature_Outdated => c"Cap2024_02_EdgeCase::All_Signature_Outdated",
            Cap2024_02_EdgeCase::Cav1_TooBig_Invalid => c"Cap2024_02_EdgeCase::Cav1_TooBig_Invalid",
            Cap2024_02_EdgeCase::Cav1_NonZeroWhenUnused => c"Cap2024_02_EdgeCase::Cav1_NonZeroWhenUnused",
            Cap2024_02_EdgeCase::Cav2_NonZeroWhenUnused_Invalid => c"Cap2024_02_EdgeCase::Cav2_NonZeroWhenUnused_Invalid",
            Cap2024_02_EdgeCase::Cav2_YSmallerThanX_Invalid => c"Cap2024_02_EdgeCase::Cav2_YSmallerThanX_Invalid",
            Cap2024_02_EdgeCase::Cav1_PushStartOutsideLimit_Invalid => c"Cap2024_02_EdgeCase::Cav1_PushStartOutsideLimit_Invalid",
            Cap2024_02_EdgeCase::Cav2_PushStartOutsideLimit_Invalid => c"Cap2024_02_EdgeCase::Cav2_PushStartOutsideLimit_Invalid",
            Cap2024_02_EdgeCase::Cav2_IgnoredBitsOfXYAreSet_Valid => c"Cap2024_02_EdgeCase::Cav2_IgnoredBitsOfXYAreSet_Valid",
        }
    }
}
const CAP2024_02_EDGE_CASES: [Cap2024_02_EdgeCase; 3] = [
    // Cap2024_02_EdgeCase::All_Signature_OffByOne,
    // Cap2024_02_EdgeCase::All_Signature_Outdated,
    Cap2024_02_EdgeCase::Cav1_NonZeroWhenUnused,
    // Cap2024_02_EdgeCase::Cav1_TooBig_IndexNonZero_Invalid,
    // Cap2024_02_EdgeCase::Cav1_TooBig_IndexZero_Valid,
    Cap2024_02_EdgeCase::Cav2_NonZeroWhenUnused_Invalid,
    Cap2024_02_EdgeCase::Cav2_YSmallerThanX_Invalid,
    // Cap2024_02_EdgeCase::Cav1_TooBig_IndexZero_Cav2_ExceedsIdentity_Invalid,
    // Cap2024_02_EdgeCase::Cav1_PushStartOutsideLimit_Invalid,
    // Cap2024_02_EdgeCase::Cav2_PushStartOutsideLimit_Invalid,
    // Cap2024_02_EdgeCase::Cav2_IgnoredBitsOfXYAreSet_Valid,
];

/// Randomly generate a range suitable for encoding as an initial resource
/// where the top never exits the encodable base region
/// so that any subsequent caveats will never push the start of the region out of encodable range
fn rand_initial_base_len<R: Rng>(rng: &mut R, initial_len_pow2: Option<u8>) -> AddrRange {
    // If we randomly generated the length directly, we would be biased towards large lengths and not test small ones (which would require a long string of 0s at the start).
    // Instead randomly choose a pow2 then sample inside that
    let log_len: u8 = initial_len_pow2.unwrap_or_else(|| ChooseRand::Between(10, 64).sample(rng));
    let len: u128 = if log_len > 64 || log_len < 10 {
        panic!("tried to create a capability >2^64 or <2^10");
    } else if log_len == 64 {
        1 << 64
    } else {
        ChooseRand::Between(1 << log_len, 1 << (log_len+1)).sample(rng)
    };

    const MAX_BASE: u128 = 1 << 64;
    let base: u64 = ChooseRand::<u64>::Between(0, (MAX_BASE - len).try_into().expect("1 << 64 - (0 < value <= 1 << 64) will always fit inside 64bit")).sample(rng);

    AddrRange::base_len(base, len)
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
fn rand_cav2_refined_base_len_from_nonidentity_cav1<R: Rng>(rng: &mut R) -> (u16, u16) {
    const MAX_RANGE: u16 = 1 << 9;
    let range_x = ChooseRand::Between(0, MAX_RANGE - 1).sample(rng);
    let range_y = ChooseRand::Between(range_x + 1, MAX_RANGE).sample(rng);

    (range_x, range_y)

}

impl<TSignaturer: Signaturer> RandCap for Cap2024_02<TSignaturer> {
    const INITIAL_LEN_POW2_RANGE: core::ops::RangeInclusive<u8> = 10..=64;

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
                let (range_x, range_y) = rand_cav2_refined_base_len_from_nonidentity_cav1(rng);
                cap = cap.with_cav2(Some(chain_cav2.into()), range_x, range_y).unwrap();
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

    type EdgeCase = Cap2024_02_EdgeCase;

    fn enum_edge_cases() -> &'static [Self::EdgeCase] {
        &CAP2024_02_EDGE_CASES
    }

    fn rand_edge_case<R: Rng>(rng: &mut R, secret: Option<u128>, secret_key_id: Option<u32>, edge_case: Self::EdgeCase) -> CapTestCase {
        let secret = secret.unwrap_or_else(|| ChooseRand::Any.sample(rng));
        let secret_key_id = secret_key_id.unwrap_or_else(|| ChooseRand::Between(0, (1 << 23) - 1).sample(rng));

        match edge_case {
            Cap2024_02_EdgeCase::All_Signature_OffByOne => todo!(),
            Cap2024_02_EdgeCase::All_Signature_Outdated => todo!(),
            Cap2024_02_EdgeCase::Cav1_NonZeroWhenUnused => {
                let chain = choose_from(rng, &ALL_0CAV_CAP_PERMS_CHAINS);
                // Start with a 0-cav capability
                let cap = {
                    let range = rand_initial_base_len(rng, None);
                    Self::new_inexact(secret, secret_key_id, range.base(), range.len(), chain.root().into()).unwrap()
                };

                let sig = cap.sig;
                let data = {
                    let mut cap_bitfield = CapBitfield::new(cap.data);
                    cap_bitfield.set_index(ChooseRand::Any.sample(rng));
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
            Cap2024_02_EdgeCase::Cav1_TooBig_Invalid => todo!(),
            Cap2024_02_EdgeCase::Cav2_NonZeroWhenUnused_Invalid => {
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

                assert_eq!(Self::from_bits(sig, data), Err(CapDecodeErr::UnexpectedCaveat), "Cav2_NonZeroWhenUnused_Invalid failure");
                
                CapTestCase {
                    secret,
                    sig,
                    data,
                    expected: Err(CapDecodeErr::UnexpectedCaveat),
                }
            },
            Cap2024_02_EdgeCase::Cav2_YSmallerThanX_Invalid => {
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
                const MAX_CAV2_CUTDOWN: u8 = 9;
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

                
                assert_eq!(Self::from_bits(sig, data), Err(CapDecodeErr::InvalidCaveat), "Cav2_YSmallerThanX_Invalid failure");
                
                CapTestCase {
                    secret,
                    sig,
                    data,
                    expected: Err(CapDecodeErr::InvalidCaveat),
                }
            },
            Cap2024_02_EdgeCase::Cav1_PushStartOutsideLimit_Invalid => todo!(),
            Cap2024_02_EdgeCase::Cav2_PushStartOutsideLimit_Invalid => todo!(),
            Cap2024_02_EdgeCase::Cav2_IgnoredBitsOfXYAreSet_Valid => todo!(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{capability::{v2024::rand::RandCap, v2024_02::Cap2024_02}, crypto::NullSig};

    #[test]
    fn generate_lots_of_caps() {
        let mut rng = rand::thread_rng();
        for _ in 0..10_000 {
            Cap2024_02::<NullSig>::rand_cap(&mut rng, None, None, None);
        }
    }

    #[test]
    fn generate_lots_of_edge_caps() {
        let mut rng = rand::thread_rng();
        for edge_case in Cap2024_02::<NullSig>::enum_edge_cases() {
            for _ in 0..10_000 {
                Cap2024_02::<NullSig>::rand_edge_case(&mut rng, None, None, *edge_case);
            }
        }
    }
}