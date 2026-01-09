//! This module concerns the encoding and decoding of external test files for other implementations e.g. Bluespec.
//

use rand::Rng;
use serde::{Deserialize, Deserializer, Serialize};

use rust_caps::{capability::{v2024::rand::{CapTestCase, RandCap}, v2024_02::Cap2024_02, v2024_11::Cap2024_11, CapDecodeErr, CapPerms}, crypto::Aes128HandrolledSig};
use serde_with::{formats::Lowercase, DeserializeAs, SerializeAs};


#[derive(
    clap::ValueEnum, Clone, Copy, Default, Debug, Serialize,
)]
#[serde(rename_all = "kebab-case")]
pub enum CapVariant {
    Cap2024_02,
    #[default]
    Cap2024_11,
}


#[derive(Clone, Copy, Debug)]
pub enum RandomCategory {
    Valid0Cav,
    Valid1Cav,
    Valid2Cav,
}

pub fn cap_initial_len_pow2s(cap: CapVariant) -> core::ops::RangeInclusive<u8> {
    match cap {
        CapVariant::Cap2024_02 => Cap2024_02::<Aes128HandrolledSig>::INITIAL_LEN_POW2_RANGE,
        CapVariant::Cap2024_11 => Cap2024_11::<Aes128HandrolledSig>::INITIAL_LEN_POW2_RANGE,
    }
}

pub fn gen_in_category<R: Rng>(rng: &mut R, cap: CapVariant, category: RandomCategory, initial_len_pow2: Option<u8>) -> CapTestCase {
    match (cap, category) {
        (CapVariant::Cap2024_02, RandomCategory::Valid0Cav) => Cap2024_02::<Aes128HandrolledSig>::rand_0cav_cap(rng, None, None, initial_len_pow2),
        (CapVariant::Cap2024_02, RandomCategory::Valid1Cav) => Cap2024_02::<Aes128HandrolledSig>::rand_1cav_cap(rng, None, None, initial_len_pow2),
        (CapVariant::Cap2024_02, RandomCategory::Valid2Cav) => Cap2024_02::<Aes128HandrolledSig>::rand_2cav_cap(rng, None, None, initial_len_pow2),
        (CapVariant::Cap2024_11, RandomCategory::Valid0Cav) => Cap2024_11::<Aes128HandrolledSig>::rand_0cav_cap(rng, None, None, initial_len_pow2),
        (CapVariant::Cap2024_11, RandomCategory::Valid1Cav) => Cap2024_11::<Aes128HandrolledSig>::rand_1cav_cap(rng, None, None, initial_len_pow2),
        (CapVariant::Cap2024_11, RandomCategory::Valid2Cav) => Cap2024_11::<Aes128HandrolledSig>::rand_2cav_cap(rng, None, None, initial_len_pow2),
    }
}

pub fn gen_edge<R: Rng>(rng: &mut R, cap: CapVariant, edge_case: usize) -> CapTestCase {
    match cap {
        CapVariant::Cap2024_02 => Cap2024_02::<Aes128HandrolledSig>::rand_edge_case(rng, None, None, Cap2024_02::<Aes128HandrolledSig>::enum_edge_cases()[edge_case]),
        CapVariant::Cap2024_11 => Cap2024_11::<Aes128HandrolledSig>::rand_edge_case(rng, None, None, Cap2024_11::<Aes128HandrolledSig>::enum_edge_cases()[edge_case]),
    } 
}

pub fn variant_edge_cases(cap: CapVariant) -> Vec<&'static core::ffi::CStr> {
    match cap {
        CapVariant::Cap2024_02 => Cap2024_02::<Aes128HandrolledSig>::enum_edge_cases().iter().map(|edge_case| (*edge_case).into()).collect(),
        CapVariant::Cap2024_11 => Cap2024_11::<Aes128HandrolledSig>::enum_edge_cases().iter().map(|edge_case| (*edge_case).into()).collect(),
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone, Copy)]
#[serde(into = "u8")]
#[serde(try_from = "u8")]
pub enum MaybeDecodeErr {
    Ok,
    Err(CapDecodeErr)
}
impl From<MaybeDecodeErr> for u8 {
    fn from(value: MaybeDecodeErr) -> Self {
        match value {
            MaybeDecodeErr::Ok => 0,
            MaybeDecodeErr::Err(CapDecodeErr::InvalidCapPermsChain) => 1,
            MaybeDecodeErr::Err(CapDecodeErr::InvalidCaveat) => 2,
            MaybeDecodeErr::Err(CapDecodeErr::InvalidSignature) => 3,
            MaybeDecodeErr::Err(CapDecodeErr::UnexpectedCaveat) => 4,
        }
    }
}
impl TryFrom<u8> for MaybeDecodeErr {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(MaybeDecodeErr::Ok),
            1 => Ok(MaybeDecodeErr::Err(CapDecodeErr::InvalidCapPermsChain)),
            2 => Ok(MaybeDecodeErr::Err(CapDecodeErr::InvalidCaveat)),
            3 => Ok(MaybeDecodeErr::Err(CapDecodeErr::InvalidSignature)),
            4 => Ok(MaybeDecodeErr::Err(CapDecodeErr::UnexpectedCaveat)),
            _ => Err(format!("Bad value for MaybeDecodeErr (expected 0-4) {value}"))
        }
    }
}

fn serialize_hex_u64<S>(v: &u64, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    serde_with::hex::Hex::<Lowercase>::serialize_as(&u64::to_be_bytes(*v), serializer)
}

fn deserialize_hex_u64<'de, D>(deserializer: D) -> Result<u64, D::Error> where D: Deserializer<'de> {
    let bytes: [u8; 8] = serde_with::hex::Hex::<Lowercase>::deserialize_as(deserializer)?;
    Ok(u64::from_be_bytes(bytes))
}

fn serialize_hex_u128<S>(v: &u128, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    serde_with::hex::Hex::<Lowercase>::serialize_as(&u128::to_be_bytes(*v), serializer)
}

fn deserialize_hex_u128<'de, D>(deserializer: D) -> Result<u128, D::Error> where D: Deserializer<'de> {
    let bytes: [u8; 16] = serde_with::hex::Hex::<Lowercase>::deserialize_as(deserializer)?;
    Ok(u128::from_be_bytes(bytes))
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct CapTestCaseSerDe {
    #[serde(serialize_with = "serialize_hex_u128", deserialize_with = "deserialize_hex_u128")]
    secret: u128,

    #[serde(serialize_with = "serialize_hex_u128", deserialize_with = "deserialize_hex_u128")]
    sig: u128,
    #[serde(serialize_with = "serialize_hex_u128", deserialize_with = "deserialize_hex_u128")]
    capability_text: u128,

    #[serde(serialize_with = "serialize_hex_u64", deserialize_with = "deserialize_hex_u64")]
    base: u64,
    #[serde(serialize_with = "serialize_hex_u128", deserialize_with = "deserialize_hex_u128")]
    top: u128,
    perms: char,
    decode_err: MaybeDecodeErr,
}
impl From<CapTestCase> for CapTestCaseSerDe {
    fn from(value: CapTestCase) -> Self {
        match value.expected {
            Ok((range, perms)) => CapTestCaseSerDe {
                secret: value.secret,
                sig: value.sig,
                capability_text: value.data,
                base: range.base(),
                top: range.top(),
                perms: match perms {
                    CapPerms::Read => 'r',
                    CapPerms::Write => 'w',
                    CapPerms::ReadWrite => 'b',
                },
                decode_err: MaybeDecodeErr::Ok,
            },
            Err(d) => CapTestCaseSerDe {
                secret: value.secret,
                sig: value.sig,
                capability_text: value.data,
                base: 0,
                top: 0,
                perms: 'b',
                decode_err: MaybeDecodeErr::Err(d)
            },
        }
    }
}

#[cfg(test)]
mod test {
    use std::error::Error;

    use csv::{ReaderBuilder, WriterBuilder};

    use crate::CapTestCaseSerDe;

    #[test]
    fn ser_deser() -> Result<(), Box<dyn Error>> {
        let test_cap_test: CapTestCaseSerDe = CapTestCaseSerDe {
            secret: 100,
            sig: 200,
            capability_text: 300,
            base: 400,
            top: 500,
            perms: 'b',
            decode_err: crate::MaybeDecodeErr::Ok,
        };

        let mut v = Vec::new();

        {
            let mut wtr = WriterBuilder::new().delimiter(b':').from_writer(&mut v);
            wtr.serialize(&test_cap_test)?;
            wtr.flush()?;
        }

        eprintln!("{}", std::str::from_utf8(&v).unwrap());

        {
            let mut rdr = ReaderBuilder::new().delimiter(b':').from_reader(v.as_slice());
            for result in rdr.deserialize() {
                let record: CapTestCaseSerDe = result?;
                assert_eq!(record, test_cap_test);
            }
        }

        Ok(())
    }
}