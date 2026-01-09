use crate::{
    capability::{
        v1::{CaveatV1ZeroInactive, CryptoCapDataV1, CryptoCapV1}, AddrRange, CapDecodeErr, CapPerms, Capability
    },
    crypto::{
        Aes128Ecb, Aes128HandrolledSig, AesGcm128Sig, MacaroonSha256HmacSig, MacaroonSha256Sig, Signaturer,
    },
};

/// Generated with OpenSSL rand
const EXAMPLE_SECRET_KEY: u128 = 0x1a72bab6_273ddf09_42c1918e_cc254f05;
const EXAMPLE_SECRET_KEY_2: u128 = 0x0aba8fe3_48051640_48487669_6ac761d9;

use packed_struct::types::SizedInteger;

#[test]
fn test_cap_constructor() {
    let base = 0;
    let length: u64 = 32;
    let log_l: u8 = 5;
    assert_eq!(length.ilog2(), log_l as u32);
    let zero_to_thirtytwo = CryptoCapDataV1::new(0, log_l);
    assert_eq!(
        zero_to_thirtytwo.get_final_base_length(),
        (base, base + length)
    )
}

#[test]
fn test_caveat_constructor() {
    let second_eighth = CaveatV1ZeroInactive::new(1, 8).unwrap();
    assert_eq!(second_eighth.i.to_primitive(), 1);
    assert_eq!(second_eighth.log_d.to_primitive(), 3);
    assert!(second_eighth.is_active());
    assert_eq!(second_eighth, CaveatV1ZeroInactive::new_log(1, 3).unwrap());
    assert_eq!(second_eighth.reduce_base_length(0x4000, 8), (0x4001, 1))
}

fn test_base_cap<TSigner: Signaturer + std::fmt::Debug>() {
    const BASE: u64 = 0x4000;
    const LENGTH: u64 = 16 << 10;
    let cap = CryptoCapV1::<TSigner>::new(EXAMPLE_SECRET_KEY, BASE, LENGTH, CapPerms::ReadWrite).unwrap();
    // When validating with the correct secret, it should work
    assert_eq!(
        cap.valid_range(EXAMPLE_SECRET_KEY),
        Ok((AddrRange::base_len_64(BASE, LENGTH), CapPerms::ReadWrite))
    );
    // When validating with the wrong secret, it shouldn't
    assert_eq!(
        cap.valid_range(EXAMPLE_SECRET_KEY_2),
        Err(CapDecodeErr::InvalidSignature)
    );
}

fn test_base_cap_and_valid_caveat<TSigner: Signaturer + std::fmt::Debug>() {
    const BASE: u64 = 0x4000;
    const LENGTH: u64 = 16 << 10;
    let cap = CryptoCapV1::<TSigner>::new(EXAMPLE_SECRET_KEY, BASE, LENGTH, CapPerms::ReadWrite).unwrap();
    // Take the last (fourth) quarter, which starts at BASE + 3/4 LENGTH
    let cap = cap.with_caveat(3, 4).unwrap();
    // When validating with the correct secret, it should work
    assert_eq!(
        cap.valid_range(EXAMPLE_SECRET_KEY),
        Ok((AddrRange::start_end_64(
            BASE + (3 * LENGTH) / 4,
            BASE + LENGTH
        ), CapPerms::ReadWrite))
    );
    // When validating with the wrong secret, it shouldn't
    assert_eq!(
        cap.valid_range(EXAMPLE_SECRET_KEY_2),
        Err(CapDecodeErr::InvalidSignature)
    );
}

fn test_base_cap_and_all_caveats<TSigner: Signaturer + std::fmt::Debug>() {
    const BASE: u64 = 0x4000;
    const LENGTH: u64 = 16 << 10;
    let cap = CryptoCapV1::<TSigner>::new(EXAMPLE_SECRET_KEY, BASE, LENGTH, CapPerms::ReadWrite).unwrap();
    // Take the first half, then the second half of the first half (second quarter), then the second half of the second half of the first half (fourth eighth)
    let cap = cap.with_caveat(0, 2).unwrap();
    let cap = cap.with_caveat(1, 2).unwrap();
    let cap = cap.with_caveat(1, 2).unwrap();
    // When validating with the correct secret, it should work
    assert_eq!(
        cap.valid_range(EXAMPLE_SECRET_KEY),
        Ok((AddrRange::start_end_64(
            BASE + (3 * LENGTH) / 8,
            BASE + LENGTH / 2
        ), CapPerms::ReadWrite))
    );
    // When validating with the wrong secret, it shouldn't
    assert_eq!(
        cap.valid_range(EXAMPLE_SECRET_KEY_2),
        Err(CapDecodeErr::InvalidSignature)
    );
}

#[test]
pub fn test_base_cap_sha256_trunc() {
    test_base_cap::<MacaroonSha256Sig>()
}

#[test]
pub fn test_base_cap_hmac_sha256_trunc() {
    test_base_cap::<MacaroonSha256HmacSig>()
}

#[test]
pub fn test_base_cap_aes_128_gcm_sig() {
    test_base_cap::<AesGcm128Sig>()
}

#[test]
pub fn test_base_cap_handrolled_aes128() {
    test_base_cap::<Aes128HandrolledSig>()
}

#[test]
pub fn test_base_cap_aes128ecb() {
    test_base_cap::<Aes128Ecb>()
}

#[test]
pub fn test_base_cap_and_valid_caveat_sha256_trunc() {
    test_base_cap_and_valid_caveat::<MacaroonSha256Sig>()
}

#[test]
pub fn test_base_cap_and_valid_caveat_hmac_sha256_trunc() {
    test_base_cap_and_valid_caveat::<MacaroonSha256HmacSig>()
}

#[test]
pub fn test_base_cap_and_valid_caveat_aes_128_gcm_sig() {
    test_base_cap_and_valid_caveat::<AesGcm128Sig>()
}

#[test]
pub fn test_base_cap_and_valid_caveat_handrolled_aes128() {
    test_base_cap_and_valid_caveat::<Aes128HandrolledSig>()
}

#[test]
pub fn test_base_cap_and_valid_caveat_aes128ecb() {
    test_base_cap_and_valid_caveat::<Aes128Ecb>()
}

#[test]
pub fn test_base_cap_and_all_caveats_sha256_trunc() {
    test_base_cap_and_all_caveats::<MacaroonSha256Sig>()
}

#[test]
pub fn test_base_cap_and_all_caveats_hmac_sha256_trunc() {
    test_base_cap_and_all_caveats::<MacaroonSha256HmacSig>()
}

#[test]
pub fn test_base_cap_and_all_caveats_aes_128_gcm_sig() {
    test_base_cap_and_all_caveats::<AesGcm128Sig>()
}

#[test]
pub fn test_base_cap_and_all_caveats_handrolled_aes128() {
    test_base_cap_and_all_caveats::<Aes128HandrolledSig>()
}

#[test]
pub fn test_base_cap_and_all_caveats_aes128ecb() {
    test_base_cap_and_all_caveats::<Aes128Ecb>()
}

#[test]
pub fn test_attempted_forgery() {
    const BASE: u64 = 0x4000;
    const LENGTH: u64 = 16;
    let cap = CryptoCapV1::<Aes128Ecb>::new(EXAMPLE_SECRET_KEY, BASE, LENGTH, CapPerms::ReadWrite).unwrap();
    // Take the first half, then the second half of the first half (second quarter), then the second half of the second half of the first half (fourth eighth)
    let cap = cap.with_caveat(0, 2).unwrap();
    let cap = cap.with_caveat(1, 2).unwrap();

    // Try to undo the last caveat by swapping out c2
    let forged_cap = CryptoCapV1::<Aes128Ecb> {
        data: CryptoCapDataV1 {
            c2: CaveatV1ZeroInactive::new(0, 2).unwrap(),
            ..cap.data
        },
        ..cap
    };
    // This shouldn't be correct with any of the secret keys
    assert_eq!(
        forged_cap.check_validity(EXAMPLE_SECRET_KEY),
        Err(CapDecodeErr::InvalidSignature)
    );
    assert_eq!(
        forged_cap.check_validity(EXAMPLE_SECRET_KEY_2),
        Err(CapDecodeErr::InvalidSignature)
    );

    // Try to undo the last caveat by swapping out c2 AND the key?
    let forged_cap = CryptoCapV1::<Aes128Ecb> {
        sig: EXAMPLE_SECRET_KEY_2,
        data: CryptoCapDataV1 {
            c2: CaveatV1ZeroInactive::new(0, 2).unwrap(),
            ..cap.data
        },
        ..cap
    };
    // This shouldn't be correct with any of the secret keys
    assert_eq!(
        forged_cap.check_validity(EXAMPLE_SECRET_KEY),
        Err(CapDecodeErr::InvalidSignature)
    );
    assert_eq!(
        forged_cap.check_validity(EXAMPLE_SECRET_KEY_2),
        Err(CapDecodeErr::InvalidSignature)
    );
}
