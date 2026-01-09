use core::ops::Range;

#[cfg(feature = "std")]
use thiserror::Error;

use crate::checked_bits::BitInt;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct AddrRange {
    start: u64,
    end: u128, // 65 bits at most TODO use checked_bits to store this?
}
impl AddrRange {
    // TODO I wish I could use .start_end() for this but we can't make it const.
    pub const FULL_ADDR_SPACE: Self = Self{ start: 0, end: 1 << 64 };

    pub fn start_end_64(start: u64, end: u64) -> Self {
        Self::start_end(start, end as u128)
    }

    pub fn start_end(start: u64, end: u128) -> Self {
        assert!(start as u128 <= end);
        // Option::unwrap and Option::expect aren't const in stable Rust yet :(
        // https://github.com/rust-lang/rust/issues/67441
        // Blocked by something to do with Drop: https://github.com/rust-lang/rust/issues/73255#issuecomment-938086246
        //
        // BitInt::check also can't be const for a similar reason - it uses shift-right to make a mask, shift-right isn't const, checked-shift-right is but it returns Option
        assert!(BitInt::<u128, 65>::check(end).is_some(), "Can't create an AddrRange with an `end` greater than 65 bits");
        Self {
            start,
            end
        }
    }

    pub fn base_len_64(base: u64, len: u64) -> Self {
        let end = base as u128 + len as u128;
        Self::start_end(base, end)
    }

    pub fn base_len(base: u64, len: u128) -> Self {
        let end = base as u128 + len;
        Self::start_end(base, end)
    }

    /// Returns true if `self` fully contains the entirety of `other`.
    ///
    /// ```
    /// # use crate::rust_caps::capability::AddrRange;
    /// assert!(AddrRange::FULL_ADDR_SPACE.encompasses(&AddrRange::FULL_ADDR_SPACE));
    /// assert!(AddrRange::FULL_ADDR_SPACE.encompasses(&AddrRange::start_end_64(0, u64::MAX)));
    /// assert!(!AddrRange::start_end_64(0, u64::MAX).encompasses(&AddrRange::FULL_ADDR_SPACE));
    /// assert!(AddrRange::start_end_64(0, 0x100).encompasses(&AddrRange::start_end_64(0x50, 0x100)));
    /// assert!(!AddrRange::start_end_64(0, 0x100).encompasses(&AddrRange::start_end_64(0x50, 0x101)));
    /// ```
    pub const fn encompasses(&self, other: &AddrRange) -> bool {
        if self.start > other.start {
            false
        } else if self.end < other.end {
            false
        } else {
            true
        }
    }

    pub const fn base(&self) -> u64 {
        self.start
    }
    pub const fn top(&self) -> u128 {
        self.end
    }
    pub const fn len(&self) -> u128 {
        self.end - (self.start as u128)
    }
}
impl From<Range<u64>> for AddrRange {
    fn from(value: Range<u64>) -> Self {
        Self::start_end(value.start, value.end as u128)
    }
}
impl core::fmt::Display for AddrRange {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "[{:x}, {:x})", self.start, self.end)
    }
}

pub trait Capability: Sized {
    type Secret;
    type Signature: PartialEq;

    /// Find the address range this capability claims to represent, without checking if the signature is valid.
    ///
    /// May return [CapabilityErr::MalformedCapability]
    fn requested_range(&self) -> Result<(AddrRange, CapPerms), CapDecodeErr>;

    /// The signature this capability has
    fn signature(&self) -> Self::Signature;

    /// Calculate the signature this capability would have if it were valid
    fn expected_signature(&self, secret: Self::Secret) -> Self::Signature;

    /// Check that this capability is valid, which requires providing the secret it was created with.
    ///
    /// May return [CapDecodeErr::InvalidSignature]
    fn check_validity(&self, secret: Self::Secret) -> Result<(), CapDecodeErr> {
        if self.expected_signature(secret) != self.signature() {
            Err(CapDecodeErr::InvalidSignature)
        } else {
            Ok(())
        }
    }

    /// If the capability is valid, which must be checked by providing the secret it was created with, return the address range it represents.
    ///
    /// May return [CapabilityErr::ValidityCheckFailed] or [CapabilityErr::MalformedCapability]
    fn valid_range(&self, secret: Self::Secret) -> Result<(AddrRange, CapPerms), CapDecodeErr> {
        self.check_validity(secret)?;
        self.requested_range()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "std", derive(Error))]
pub enum CapEncodeErr {
    #[cfg_attr(feature = "std", error("Supplied base range is not representable"))]
    UnrepresentableBaseRange,
    #[cfg_attr(feature = "std", error("Supplied caveat is not representable"))]
    UnrepresentableCaveat,
    #[cfg_attr(feature = "std", error("Trying to apply a caveat that pushes the range out-of-bounds"))]
    InvalidCaveat,
    #[cfg_attr(feature = "std", error("No caveat slots left to store a new caveat"))]
    NoCaveatsLeft,
    #[cfg_attr(feature = "std", error("Can't shrink permissions to {0}"))]
    CantShrinkPerms(CapPerms),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "std", derive(Error))]
pub enum CapDecodeErr {
    #[cfg_attr(feature = "std", error("Supplied caveat is invalid"))]
    InvalidCaveat,
    #[cfg_attr(feature = "std", error("Validity check failed - capability has been tampered with, or the wrong secret was provided"))]
    InvalidSignature,
    #[cfg_attr(feature = "std", error("Invalid CapPermsChain value"))]
    InvalidCapPermsChain,
    #[cfg_attr(feature = "std", error("An unexpected caveat was supplied"))]
    UnexpectedCaveat,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "std", derive(Error))]
pub enum CapEncDecErr {
    #[cfg_attr(feature = "std", error("Enc: {0}"))]
    Enc(CapEncodeErr),
    #[cfg_attr(feature = "std", error("Dec: {0}"))]
    Dec(CapDecodeErr),
}
impl From<CapEncodeErr> for CapEncDecErr {
    fn from(value: CapEncodeErr) -> Self {
        Self::Enc(value)
    }
}
impl From<CapDecodeErr> for CapEncDecErr {
    fn from(value: CapDecodeErr) -> Self {
        Self::Dec(value)
    }
}


#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CapPerms {
    Read,
    Write,
    ReadWrite,
}
impl CapPerms {
    pub fn read(&self) -> bool {
        match self {
            Self::Read | Self::ReadWrite => true,
            Self::Write => false,
        }
    }
    pub fn write(&self) -> bool {
        match self {
            Self::Write | Self::ReadWrite => true,
            Self::Read => false,
        }
    }
}
impl core::fmt::Display for CapPerms {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            CapPerms::Read => write!(f, "Read"),
            CapPerms::Write => write!(f, "Write"),
            CapPerms::ReadWrite => write!(f, "ReadWrite"),
        }
    }
}

#[cfg(feature = "std")]
pub mod v1;
pub mod v2024;
pub mod v2024_02;
pub mod v2024_11;