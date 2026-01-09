use openssl::hash::MessageDigest;
use openssl::pkey::PKey;
use openssl::sha::sha256;
use openssl::sign::Signer;
use openssl::symm::{encrypt_aead, Cipher, Crypter};

use crate::util::{byte_array_to_u128, byte_slice_to_u128};

/// SHA256-hash of some data, truncated to u128
pub fn sha256_trunc(data: &[u8]) -> u128 {
    byte_slice_to_u128(&sha256(data)[..16]).unwrap()
}

/// SHA256-HMAC of a message with a key, with the output truncated to u128
pub fn sha256_hmac_trunc(key: &[u8], msg: &[u8]) -> u128 {
    let key = PKey::hmac(key).unwrap();
    let mut signer = Signer::new(MessageDigest::sha256(), &key).unwrap();
    signer.update(msg).unwrap();
    let hmac = signer.sign_to_vec().unwrap();
    byte_slice_to_u128(&hmac[..16]).unwrap()
}

/// AES-128-GCM authenticated MAC - uses the message as Additionally Authenticated Data without
/// encrypting anything
pub fn aes_gcm_128_auth(key: &[u8], aad: &[u8]) -> u128 {
    let mut tag = [0; 16];
    encrypt_aead(
        Cipher::aes_128_gcm(),
        key,
        Some(&[0u8; 12]), // Not the same as None
        aad,
        &[],
        &mut tag,
    )
    .expect("AES-128 failed");
    byte_array_to_u128(&tag)
}

/// AES-128-ECB - should be equivalent to handrolled AES, ECB method is just to not use an IV at all
pub fn aes_128_ecb(key: &[u8], data: &[u8]) -> u128 {
    let mut crypter = Crypter::new(
        Cipher::aes_128_ecb(),
        openssl::symm::Mode::Encrypt,
        &key,
        None,
    )
    .unwrap();
    crypter.pad(false);
    let mut openssl_result_bytes = [0; 32];
    let mut count = crypter.update(&data, &mut openssl_result_bytes).unwrap();
    count += crypter.finalize(&mut openssl_result_bytes).unwrap();
    assert_eq!(count, 16);
    byte_slice_to_u128(&openssl_result_bytes[0..count]).unwrap()
}

#[cfg(test)]
mod test {
    use assert_hex::assert_eq_hex;

    use crate::{crypto::openssl::*, util::le_literal};

    #[test]
    pub fn test_sha256_trunc() {
        // from https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/SHA256.pdf
        let full_expected_digest_top: u128 = le_literal(0xBA7816BF_8F01CFEA_414140DE_5DAE2223);
        let _full_expected_digest_bottom: u128 = le_literal(0xB00361A3_96177A9C_B410FF61_F20015AD);
        let data = b"abc";
        assert_eq_hex!(sha256_trunc(data), full_expected_digest_top);
    }

    #[test]
    pub fn test_sha256_hmac_trunc() {
        // from https://en.wikipedia.org/wiki/HMAC
        let full_expected_digest_top: u128 = le_literal(0xf7bc83f4_30538424_b13298e6_aa6fb143);
        let _full_expected_digest_bottom: u128 = le_literal(0xef4d59a1_49461759_97479dbc_2d1a3cd8);
        let key = b"key";
        let msg = b"The quick brown fox jumps over the lazy dog";
        assert_eq_hex!(sha256_hmac_trunc(key, msg), full_expected_digest_top);
    }

    #[test]
    pub fn test_aes_gcm_128_auth() {
        // from https://csrc.nist.rip/groups/ST/toolkit/BCM/documents/proposedmodes/gcm/gcm-spec.pdf
        let full_expected_digest: u128 = le_literal(0x58e2fccefa7e3061367f1d57a4e7455a);
        let key = [0u8; 16];
        let msg = b"";
        assert_eq_hex!(aes_gcm_128_auth(&key, msg), full_expected_digest);
    }
}
