use std::marker::PhantomData;

use packed_struct::prelude::*;
use thiserror::Error;

use crate::{capability::CapEncodeErr, crypto::Signaturer};

use super::{AddrRange, CapDecodeErr, CapPerms, Capability};

#[cfg(test)]
mod test;

#[derive(Error, Debug, PartialEq, Eq)]
enum BadCaveatReason {
    #[error("Caveats enabled out of order: c1 {c1} c2 {c2} c3 {c3}")]
    CaveatsOutOfOrder{ c1: bool, c2: bool, c3: bool },
    #[error("Bits of i above d set, when they shouldn't be")]
    CaveatTopBitsSet,
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

#[derive(Debug)]
pub struct CryptoCapV1<TSigner: Signaturer + std::fmt::Debug> {
    pub sig: u128,
    pub data: CryptoCapDataV1,
    pub signer: PhantomData<TSigner>,
}
impl<TSigner: Signaturer + std::fmt::Debug> CryptoCapV1<TSigner> {
    fn sign_data(data: CryptoCapDataV1, secret: u128) -> u128 {
        let pub_data = data.pack_to_vec().unwrap();
        TSigner::sign(&pub_data, secret)
    }

    fn new_from_log(b_mult: u64, log_l: u8, secret: u128) -> Self {
        let data = CryptoCapDataV1::new(b_mult, log_l);
        let sig = Self::sign_data(data, secret);
        Self {
            sig,
            data,
            signer: PhantomData::default(),
        }
    }
}
impl<TSigner: Signaturer + std::fmt::Debug> CryptoCapV1<TSigner> {
    pub fn new(secret: u128, base: u64, length: u64, perms: CapPerms) -> Result<Self, CapEncodeErr> {
        if base % length != 0 {
            return Err(CapEncodeErr::UnrepresentableBaseRange);
        }
        if perms != CapPerms::ReadWrite {
            return Err(CapEncodeErr::CantShrinkPerms(perms));
        }
        let log_l = length
            .checked_ilog2()
            .ok_or(CapEncodeErr::UnrepresentableBaseRange)?;
        Ok(Self::new_from_log(
            base / length,
            log_l
                .try_into()
                .expect("log2 of 64-bit value must be less than 255"),
            secret,
        ))
    }

    /// Make a copy of a capability with a *caveat* that reduces the range to (i/d, i+1/d) of the previous range.
    ///
    /// May return [CapabilityErr::UnrepresentableCaveat], [CapabilityErr::NoCaveatsLeft], or [CapabilityErr::MalformedCapability]
    pub fn with_caveat(&self, i: u16, d: u16) -> Result<Self, CapEncodeErr> {
        let data = self.data.try_add_caveat(i, d)?;
        data.is_well_formed()?;
        Ok(Self {
            sig: Self::sign_data(data, self.sig),
            data,
            signer: self.signer,
        })
    }
}

impl<TSigner: Signaturer + std::fmt::Debug> Capability for CryptoCapV1<TSigner> {
    type Secret = u128;
    type Signature = u128;

    fn requested_range(&self) -> Result<(AddrRange, CapPerms), CapDecodeErr> {
        self.data.is_well_formed()?;
        let (base, len) = self.data.get_final_base_length();
        Ok((AddrRange::base_len_64(base, len), CapPerms::ReadWrite))
    }

    fn signature(&self) -> Self::Signature {
        self.sig
    }

    fn expected_signature(&self, secret: Self::Secret) -> Self::Signature {
        // Base signature - the capability without any caveats
        let mut sig = Self::sign_data(
            CryptoCapDataV1 {
                c1: CaveatV1ZeroInactive::new_inactive(),
                c2: CaveatV1ZeroInactive::new_inactive(),
                c3: CaveatV1ZeroInactive::new_inactive(),
                ..self.data
            },
            secret,
        );
        if self.data.c1.is_active() {
            // If c1 is active, resign with c1's data and c2/c3 zeroed
            sig = Self::sign_data(
                CryptoCapDataV1 {
                    c2: CaveatV1ZeroInactive::new_inactive(),
                    c3: CaveatV1ZeroInactive::new_inactive(),
                    ..self.data
                },
                sig,
            );
        }
        if self.data.c2.is_active() {
            // If c2 is active, resign with c1 and c2's data and c3 zeroed
            sig = Self::sign_data(
                CryptoCapDataV1 {
                    c3: CaveatV1ZeroInactive::new_inactive(),
                    ..self.data
                },
                sig,
            );
        }
        if self.data.c3.is_active() {
            // If c3 is active, resign with all data
            sig = Self::sign_data(self.data, sig);
        }

        sig
    }
}

#[derive(PackedStruct, Debug, Clone, Copy, PartialEq, Eq)]
#[packed_struct(endian = "msb", bit_numbering = "msb0", size_bytes = 16)]
pub struct PackableU128 {
    #[packed_field(bits = "0..64")]
    bottom: u64,
    #[packed_field(bits = "64..128")]
    top: u64,
}
impl From<PackableU128> for u128 {
    fn from(x: PackableU128) -> Self {
        ((x.top as u128) << 64) | (x.bottom as u128)
    }
}
impl From<u128> for PackableU128 {
    fn from(x: u128) -> Self {
        Self {
            bottom: x as u64,
            top: (x >> 64) as u64,
        }
    }
}

#[derive(PackedStruct, Debug, Clone, Copy, PartialEq, Eq)]
#[packed_struct(endian = "msb", bit_numbering = "msb0", size_bytes = 16)]
pub struct CryptoCapDataV1 {
    #[packed_field(bits = "0..56")]
    pub b_mult: Integer<u64, packed_bits::Bits<56>>,
    #[packed_field(bits = "56..64")]
    pub log_l: u8,
    #[packed_field(bits = "64..83")]
    pub c1: CaveatV1ZeroInactive,
    #[packed_field(bits = "83..102")]
    pub c2: CaveatV1ZeroInactive,
    #[packed_field(bits = "102..121")]
    pub c3: CaveatV1ZeroInactive,
    #[packed_field(bits = "121..128")]
    pub misc: Integer<u8, packed_bits::Bits<7>>,
}
impl CryptoCapDataV1 {
    pub fn new(b_mult: u64, log_l: u8) -> Self {
        Self {
            b_mult: b_mult.try_into().unwrap(),
            log_l,
            c1: CaveatV1ZeroInactive::new_inactive(),
            c2: CaveatV1ZeroInactive::new_inactive(),
            c3: CaveatV1ZeroInactive::new_inactive(),
            misc: 0.into(),
        }
    }

    /// Check if the capability is well-formed i.e. there are no bit patterns that don't make sense
    fn is_well_formed(&self) -> Result<(), BadCaveatReason> {
        self.c1.is_well_formed()?;
        self.c2.is_well_formed()?;
        self.c3.is_well_formed()?;

        // c2 can only be active if c1 is active,
        // c3 can only be active if c1 and c2 are active
        if self.c3.is_active() && !self.c2.is_active() {
            Err(BadCaveatReason::CaveatsOutOfOrder { c1: self.c1.is_active(), c2: self.c2.is_active(), c3: self.c3.is_active() })
        } else if self.c2.is_active() && !self.c1.is_active() {
            Err(BadCaveatReason::CaveatsOutOfOrder { c1: self.c1.is_active(), c2: self.c2.is_active(), c3: self.c3.is_active() })
        } else {
            Ok(())
        }
    }

    pub fn try_add_caveat(self, i: u16, d: u16) -> Result<Self, CapEncodeErr> {
        let cav = CaveatV1ZeroInactive::new(i, d)?;

        if self.c1.is_active() {
            if self.c2.is_active() {
                if self.c3.is_active() {
                    Err(CapEncodeErr::NoCaveatsLeft)
                } else {
                    Ok(Self { c3: cav, ..self })
                }
            } else {
                Ok(Self { c2: cav, ..self })
            }
        } else {
            Ok(Self { c1: cav, ..self })
        }
    }

    pub fn get_final_base_length(&self) -> (u64, u64) {
        assert!(self.log_l <= 64); // TODO could use 256-bit shifting if we really wanted to
        let initial_length = 1u64 << self.log_l;
        let initial_base = self.b_mult.to_primitive() * initial_length;

        let (mut base, mut length) = self.c1.reduce_base_length(initial_base, initial_length);
        (base, length) = self.c2.reduce_base_length(base, length);
        self.c3.reduce_base_length(base, length)
    }
}

/// Encodes a caveat as the subrange (i/d, i+1/d) of the previous (base:length).
///
/// d is always a power of two, thus log2(d) is encoded in 4 bits providing a maximum divisor of 2^15.
/// When d=0, the caveat is inactive.
///
/// i is encoded as a 15-bit unsigned integer. The maximum value of i is (d-1) = 2^log2(d) - 1,
/// therefore only the bottom log2(d) bits of i may be set. This canbe tested using `(i >> log2(d)) == 0`.
///
/// ```text
/// logd            i
/// 0010    000 0000 0000 0011
///
/// d=2^2=4   max(i) = 3 => bottom 2 bits = bottom log2(d) bits may be set
/// ```
#[derive(PackedStruct, Debug, Clone, Copy, PartialEq, Eq)]
#[packed_struct(endian = "msb", bit_numbering = "msb0")]
pub struct CaveatV1ZeroInactive {
    #[packed_field(bits = "0..4")]
    log_d: Integer<u8, packed_bits::Bits<4>>,
    #[packed_field(bits = "4..19")]
    i: Integer<u16, packed_bits::Bits<15>>,
}
impl CaveatV1ZeroInactive {
    fn new_inactive() -> Self {
        Self {
            log_d: 0.into(),
            i: 0.into(),
        }
    }

    pub fn new(i: u16, d: u16) -> Result<Self, CapEncodeErr> {
        if i >= d || d == 0 || !d.is_power_of_two() {
            Err(CapEncodeErr::UnrepresentableCaveat)
        } else {
            let log_d = d.ilog2();
            Self::new_log(i, log_d as u8)
        }
    }

    fn new_log(i: u16, log_d: u8) -> Result<Self, CapEncodeErr> {
        if log_d > 15 || (i >> log_d) > 0 {
            Err(CapEncodeErr::UnrepresentableCaveat)
        } else {
            Ok(Self {
                log_d: log_d.try_into().unwrap(),
                i: i.try_into().unwrap(),
            })
        }
    }

    fn is_active(&self) -> bool {
        // If log_d is 0, the caveat is a divisor by 1
        // All bits of i beyond bit #log_d are unused and should be zero, but we don't need to ensure that
        self.log_d.to_primitive() != 0u8
        // && ((self.i.to_primitive() >> self.log_d.to_primitive()) == 0)
    }

    fn is_well_formed(&self) -> Result<(), BadCaveatReason> {
        if (self.i.to_primitive() >> self.log_d.to_primitive()) == 0 {
            Ok(())
        } else {
            Err(BadCaveatReason::CaveatTopBitsSet)
        }
    }

    fn reduce_base_length(&self, base: u64, length: u64) -> (u64, u64) {
        if self.is_active() {
            let length = length >> self.log_d.to_primitive();
            let base = base + (self.i.to_primitive() as u64) * length;
            (base, length)
        } else {
            (base, length)
        }
    }
}
