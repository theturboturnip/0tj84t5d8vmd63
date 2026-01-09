use crate::util::byte_slice_to_u128;
#[cfg(feature="openssl")]
use crate::util::u128_to_byte_array;

pub mod handrolled;
#[cfg(feature="openssl")]
mod openssl;

/// Crypto-capabilities require a signature function, which takes public data and a secret and produces
/// a signature.
///
/// The signature and data are both made public, but the signature must obscure the original secret
/// even when different data is signed multiple times with the same secret.
///
/// The purpose is to prove a new capability/data was generated from the secret without revealing said secret.
pub trait Signaturer: core::fmt::Debug + Clone + Eq {
    fn sign(pub_data: &[u8], secret: u128) -> u128;
}

/// The Macaroon paper uses SHA-256-based HMAC.
/// This internally requires a 512-bit secret key and produces a 256-bit hash,
/// but the key and hash are both truncated for the sake of the smaller hardware capabilities.
///
/// DO NOT USE IN PRODUCTION - the security of using a truncated key is iffy
#[cfg(feature="openssl")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacaroonSha256HmacSig {}
#[cfg(feature="openssl")]
impl Signaturer for MacaroonSha256HmacSig {
    fn sign(pub_data: &[u8], secret: u128) -> u128 {
        openssl::sha256_hmac_trunc(&u128_to_byte_array(secret), pub_data)
    }
}

/// If SHA-256-HMAC is too performance intensive,
/// the Macaroon paper recommends just using SHA-256 instead.
#[cfg(feature="openssl")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacaroonSha256Sig {}
#[cfg(feature="openssl")]
impl Signaturer for MacaroonSha256Sig {
    fn sign(pub_data: &[u8], secret: u128) -> u128 {
        let mut data_concat = vec![];
        data_concat.extend_from_slice(&u128_to_byte_array(secret));
        data_concat.extend_from_slice(pub_data);
        openssl::sha256_trunc(&data_concat)
    }
}

/// I initially thought it would be useful to do an AES-GCM MAC.
/// Not sure if we really need a true MAC function -
/// indeed the [MacaroonSha256Sig] implies a hash works just as well.
#[cfg(feature="openssl")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AesGcm128Sig {}
#[cfg(feature="openssl")]
impl Signaturer for AesGcm128Sig {
    fn sign(pub_data: &[u8], secret: u128) -> u128 {
        openssl::aes_gcm_128_auth(&u128_to_byte_array(secret), pub_data)
    }
}

#[cfg(feature="openssl")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Aes128Ecb {}
#[cfg(feature="openssl")]
impl Signaturer for Aes128Ecb {
    fn sign(pub_data: &[u8], secret: u128) -> u128 {
        openssl::aes_128_ecb(&u128_to_byte_array(secret), pub_data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Aes128HandrolledSig {}
impl Signaturer for Aes128HandrolledSig {
    fn sign(pub_data: &[u8], secret: u128) -> u128 {
        handrolled::aes::aes128(byte_slice_to_u128(pub_data).unwrap(), secret)
    }
}

/// ONLY USE THIS IN TESTS - useful for skipping signature encryption when it's not necessary
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NullSig {}
impl Signaturer for NullSig {
    fn sign(_pub_data: &[u8], secret: u128) -> u128 {
        secret
    }
}