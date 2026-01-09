#![cfg_attr(not(feature = "std"), no_std)]

use paste::paste;
use rust_caps::{
    capability::{
        v2024::{perms::CapPermsChain, Cap2024},
        v2024_02::Cap2024_02,
        v2024_11::Cap2024_11,
        AddrRange, CapDecodeErr, CapEncodeErr, Capability,
    },
    crypto::Aes128HandrolledSig,
};

pub use rust_caps::capability::CapPerms;

/// One of the flags that can be set in [CCapNativeVirtqDesc],
/// equivalent to `VIRTQ_DESC_F_NEXT`
pub const CCAP_VIRTQ_F_NEXT: u16 = 1 << 0;
/// One of the flags that can be set in [CCapNativeVirtqDesc],
/// equivalent to `VIRTQ_DESC_F_WRITE`
pub const CCAP_VIRTQ_F_WRITE: u16 = 1 << 1;
/// One of the flags that can be set in [CCapNativeVirtqDesc],
/// equivalent to `VIRTQ_DESC_F_INDIRECT`
pub const CCAP_VIRTQ_F_INDIRECT: u16 = 1 << 2;

/// A struct matching the virtio virtqueue descriptor layout except the data is native-byte-order
/// instead of little-endian.
/// On a little-endian machine it should be possible to cast a pointer to this struct directly to whatever
/// virtio descriptor struct you have in your own C code.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CCapNativeVirtqDesc {
    addr: u64,
    len: u32,
    /// Must be 3-bits, a bitfield of [CCAP_VIRTQ_F_NEXT], [CCAP_VIRTQ_F_WRITE], and [CCAP_VIRTQ_F_INDIRECT]
    flags: u16,
    /// Must be 13-bits, will be packed into bits of the secret_id
    next: u16,
}

/// Little-endian representation of a 128-bit number
#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct CCapU128([u8; 16]);

impl From<u128> for CCapU128 {
    fn from(value: u128) -> Self {
        Self(value.to_le_bytes())
    }
}
impl From<CCapU128> for u128 {
    fn from(value: CCapU128) -> Self {
        u128::from_le_bytes(value.0)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CCap2024_02 {
    signature: CCapU128,
    data: CCapU128,
}

impl From<Cap2024_02<Aes128HandrolledSig>> for CCap2024_02 {
    fn from(value: Cap2024_02<Aes128HandrolledSig>) -> Self {
        Self {
            signature: value.sig.into(),
            data: value.data.into(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CCap2024_11 {
    signature: CCapU128,
    data: CCapU128,
}

impl From<Cap2024_11<Aes128HandrolledSig>> for CCap2024_11 {
    fn from(value: Cap2024_11<Aes128HandrolledSig>) -> Self {
        Self {
            signature: value.sig.into(),
            data: value.data.into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum CCapPerms {
    Read = 0b01,
    Write = 0b10,
    ReadWrite = 0b11,
}
impl TryFrom<u8> for CCapPerms {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0b01 => Ok(CCapPerms::Read),
            0b10 => Ok(CCapPerms::Write),
            0b11 => Ok(CCapPerms::ReadWrite),
            _ => Err(()),
        }
    }
}
impl From<CapPerms> for CCapPerms {
    fn from(value: CapPerms) -> Self {
        match value {
            CapPerms::Read => CCapPerms::Read,
            CapPerms::Write => CCapPerms::Write,
            CapPerms::ReadWrite => CCapPerms::ReadWrite,
        }
    }
}
impl From<CCapPerms> for CapPerms {
    fn from(value: CCapPerms) -> Self {
        match value {
            CCapPerms::Read => CapPerms::Read,
            CCapPerms::Write => CapPerms::Write,
            CCapPerms::ReadWrite => CapPerms::ReadWrite,
        }
    }
}

/// Convert a CCapPerms enum to a null-terminated static immutable C string.
/// Invokes undefined behaviour if passed an value not in the enumeration.
#[no_mangle]
pub unsafe extern "C" fn ccap_perms_str(perms: CCapPerms) -> *const core::ffi::c_char {
    match perms {
        CCapPerms::Read => "CCapPerms::Read\0".as_ptr() as *const i8,
        CCapPerms::Write => "CCapPerms::Write\0".as_ptr() as *const i8,
        CCapPerms::ReadWrite => "CCapPerms::ReadWrite\0".as_ptr() as *const i8,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[repr(i32)]
pub enum CCapResult {
    Success = 0,
    Encode_UnrepresentableBaseRange = 1,
    Encode_UnrepresentableCaveat = 2,
    Encode_InvalidCaveat = 3,
    Encode_NoCaveatsLeft = 4,
    Encode_CantShrinkPerms = 5,
    Decode_InvalidCaveat = 6,
    Decode_InvalidSignature = 7,
    Decode_InvalidCapPermsChain = 8,
    Decode_UnexpectedCaveat = 9,
    Encode_TooBigSecretId = 10,
    Encode_InvalidPerms = 11,
    Decode_NotVirtio = 12,
    CatastrophicFailure = 13,
    NullRequiredArgs = 100,
}

/// Convert a CCapResult enum to a null-terminated static immutable C string.
/// Invokes undefined behaviour if passed an value not in the enumeration.
#[no_mangle]
pub unsafe extern "C" fn ccap_result_str(res: CCapResult) -> *const core::ffi::c_char {
    match res {
        CCapResult::Success => "CCapResult::Success\0".as_ptr() as *const i8,
        CCapResult::Encode_UnrepresentableBaseRange => {
            "CCapResult::Encode_UnrepresentableBaseRange\0".as_ptr() as *const i8
        }
        CCapResult::Encode_UnrepresentableCaveat => {
            "CCapResult::Encode_UnrepresentableCaveat\0".as_ptr() as *const i8
        }
        CCapResult::Encode_InvalidCaveat => {
            "CCapResult::Encode_InvalidCaveat\0".as_ptr() as *const i8
        }
        CCapResult::Encode_NoCaveatsLeft => {
            "CCapResult::Encode_NoCaveatsLeft\0".as_ptr() as *const i8
        }
        CCapResult::Encode_CantShrinkPerms => {
            "CCapResult::Encode_CantShrinkPerms\0".as_ptr() as *const i8
        }
        CCapResult::Decode_InvalidCaveat => {
            "CCapResult::Decode_InvalidCaveat\0".as_ptr() as *const i8
        }
        CCapResult::Decode_InvalidSignature => {
            "CCapResult::Decode_InvalidSignature\0".as_ptr() as *const i8
        }
        CCapResult::Decode_InvalidCapPermsChain => {
            "CCapResult::Decode_InvalidCapPermsChain\0".as_ptr() as *const i8
        }
        CCapResult::Decode_UnexpectedCaveat => {
            "CCapResult::Decode_UnexpectedCaveat\0".as_ptr() as *const i8
        }
        CCapResult::Encode_TooBigSecretId => {
            "CCapResult::Encode_TooBigSecretId\0".as_ptr() as *const i8
        }
        CCapResult::Encode_InvalidPerms => {
            "CCapResult::Encode_InvalidPerms\0".as_ptr() as *const i8
        }
        CCapResult::Decode_NotVirtio => "CCapResult::Decode_NotVirtio\0".as_ptr() as *const i8,
        CCapResult::NullRequiredArgs => "CCapResult::NullRequiredArgs\0".as_ptr() as *const i8,
        CCapResult::CatastrophicFailure => {
            "CCapResult::CatastrophicFailure\0".as_ptr() as *const i8
        }
    }
}

impl From<CapEncodeErr> for CCapResult {
    fn from(value: CapEncodeErr) -> Self {
        match value {
            CapEncodeErr::UnrepresentableBaseRange => CCapResult::Encode_UnrepresentableBaseRange,
            CapEncodeErr::UnrepresentableCaveat => CCapResult::Encode_UnrepresentableCaveat,
            // OOB caveat
            CapEncodeErr::InvalidCaveat => CCapResult::Encode_InvalidCaveat,
            CapEncodeErr::NoCaveatsLeft => CCapResult::Encode_NoCaveatsLeft,
            CapEncodeErr::CantShrinkPerms(_) => CCapResult::Encode_CantShrinkPerms,
        }
    }
}
impl From<CapDecodeErr> for CCapResult {
    fn from(value: CapDecodeErr) -> Self {
        match value {
            CapDecodeErr::InvalidCaveat => CCapResult::Decode_InvalidCaveat,
            CapDecodeErr::InvalidSignature => CCapResult::Decode_InvalidSignature,
            CapDecodeErr::InvalidCapPermsChain => CCapResult::Decode_InvalidCapPermsChain,
            CapDecodeErr::UnexpectedCaveat => CCapResult::Decode_UnexpectedCaveat,
        }
    }
}

/// Define the API in a rust macro so we can reproduce it for multiple different capability format versions
macro_rules! ccap_api {
    ($prefix:ident, $ctype:ty, $rtype:ty) => {
        paste! {

#[allow(non_upper_case_globals)]
const [< $prefix _max_secret_id >]: u32 = (1 << 23) - 1;

/// Initialize a capability from scratch allowing access to the full 64-bit address range (base = 0, len = 1<<64), given the permissions (Read|Write|Both), and the secret id.
/// Calculates the capability signature given the packed data and the secret.
///
/// cap and secret are non-optional, and the function returns `NullRequiredArgs` if either are null.
///
/// Returns `Encode_TooBigSecretId` if `secret_id` does not fit into 23 bits.
///
/// Returns `Encode_InvalidPerms` if `perms` is not a member of the `CCapPerms` enum.
///
/// Does not use caveats.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _init_almighty >](cap: *mut $ctype, secret: *const CCapU128, secret_id: u32, perms: u8) -> CCapResult {
    if cap.is_null() || secret.is_null() {
        return CCapResult::NullRequiredArgs;
    }

    // Check that perms is a valid permutation member
    let perms: CCapPerms = match perms.try_into() {
        Ok(perms) => perms,
        Err(_) => return CCapResult::Encode_InvalidPerms,
    };

    // Check that the secret_id is in bounds, return an encode error
    if (secret_id > [< $prefix _max_secret_id >]) {
        return CCapResult::Encode_TooBigSecretId;
    }

    let rust_cap = $rtype::new(
        (*secret).into(),
        secret_id,
        0,
        1 << 64,
        perms.into()
    );
    match rust_cap {
        Ok(rust_cap) => {
            *cap = rust_cap.into();
            CCapResult::Success
        }
        Err(err) => err.into(),
    }
}

/// Initialize a capability from scratch, given the contiguous memory range it grants access to, the permissions (Read|Write|Both), and the secret_id.
/// Only uses the initial resource.
/// Calculates the capability signature given the packed data and the secret.
///
/// cap and secret are non-optional, and the function returns `NullRequiredArgs` if either are null.
///
/// Returns an Encode error if the base/length is not exactly representable.
/// Use [ccap$version_init_inexact] to allow rounding the bounds up in this case instead of returning an error.
///
/// Returns `Encode_TooBigSecretId` if `secret_id` does not fit into 23 bits.
///
/// Returns `Encode_InvalidPerms` if `perms` is not a member of the `CCapPerms` enum.
///
/// Does not use caveats.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _init_exact >](cap: *mut $ctype, secret: *const CCapU128, base: u64, len: u64, secret_id: u32, perms: CCapPerms) -> CCapResult {
    if cap.is_null() || secret.is_null() {
        return CCapResult::NullRequiredArgs;
    }

    // Check that perms is a valid permutation member
    let perms: CCapPerms = match perms.try_into() {
        Ok(perms) => perms,
        Err(_) => return CCapResult::Encode_InvalidPerms,
    };

    // Check that the secret_id is in bounds, return an encode error
    if (secret_id > [< $prefix _max_secret_id >]) {
        return CCapResult::Encode_TooBigSecretId;
    }

    let rust_cap = $rtype::new(
        (*secret).into(),
        secret_id,
        base,
        len.into(),
        perms.into()
    );
    match rust_cap {
        Ok(rust_cap) => {
            *cap = rust_cap.into();
            CCapResult::Success
        }
        Err(err) => err.into(),
    }
}

/// Initialize a capability from scratch, given the contiguous memory range it grants access to, the permissions (Read|Write|Both), and the secret_id.
/// Uses the initial resource and both caveats if necessary.
/// Calculates the capability signature given the packed data and the secret.
///
/// cap and secret are non-optional, and the function returns `NullRequiredArgs` if either are null.
///
/// Returns an Encode error if the base/length is not exactly representable.
/// Use [ccap$version_init_inexact] to allow rounding the bounds up in this case instead of returning an error.
///
/// Returns `Encode_TooBigSecretId` if `secret_id` does not fit into 23 bits.
///
/// Returns `Encode_InvalidPerms` if `perms` is not a member of the `CCapPerms` enum.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _init_cavs_exact >](cap: *mut $ctype, secret: *const CCapU128, base: u64, len: u64, secret_id: u32, perms: CCapPerms) -> CCapResult {
    if cap.is_null() || secret.is_null() {
        return CCapResult::NullRequiredArgs;
    }

    // Check that perms is a valid permutation member
    let perms: CCapPerms = match perms.try_into() {
        Ok(perms) => perms,
        Err(_) => return CCapResult::Encode_InvalidPerms,
    };

    // Check that the secret_id is in bounds, return an encode error
    if (secret_id > [< $prefix _max_secret_id >]) {
        return CCapResult::Encode_TooBigSecretId;
    }

    match internal_init_cavs::<$rtype>((*secret).into(), base, len, secret_id, perms) {
        (Some(rust_cap), CCapResult::Success) => {
            *cap = rust_cap.into();
            return CCapResult::Success
        }
        (None, CCapResult::Success) => unreachable!(),
        (_, err) => return err,
    }

}


/// Initialize a capability from scratch, given the contiguous memory range it grants access to, the permissions (Read|Write|Both), and the secret_id.
/// Calculates the capability signature given the packed data and the secret.
///
/// cap and secret are non-optional, and the function returns `NullRequiredArgs` if either are null.
///
/// Will round the bounds up to the smallest possible value that encloses [base, base+len].
/// If exact bounds are required use [ccap$version_init_exact].
///
/// Returns `Encode_TooBigSecretId` if `secret_id` does not fit into 23 bits.
///
/// Returns `Encode_InvalidPerms` if `perms` is not a member of the `CCapPerms` enum.
///
/// Does not use caveats.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _init_inexact >](cap: *mut $ctype, secret: *const CCapU128, base: u64, len: u64, secret_id: u32, perms: CCapPerms) -> CCapResult {
    if cap.is_null() || secret.is_null() {
        return CCapResult::NullRequiredArgs;
    }

    // Check that perms is a valid permutation member
    let perms: CCapPerms = match perms.try_into() {
        Ok(perms) => perms,
        Err(_) => return CCapResult::Encode_InvalidPerms,
    };

    // Check that the secret_id is in bounds, return an encode error
    if (secret_id > [< $prefix _max_secret_id >]) {
        return CCapResult::Encode_TooBigSecretId;
    }

    let rust_cap = $rtype::new_inexact(
        (*secret).into(),
        secret_id,
        base,
        len.into(),
        perms.into()
    );
    match rust_cap {
        Ok(rust_cap) => {
            *cap = rust_cap.into();
            CCapResult::Success
        }
        Err(err) => err.into(),
    }
}


/// Check if a capability has a valid signature, assuming it was encrypted with the given secret.
///
/// cap and secret are non-optional, and the function returns `CCapResult_NullRequiredArgs` if either are null.
///
/// Returns `CCapResult_Success` if the signature is valid.
/// Returns `CCapResult_DecodeInvalidSignature` if the signature is invalid.
/// Returns other errors if the capability is otherwise malformed.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _check_signature >](cap: *const $ctype, secret: *const CCapU128) -> CCapResult {
    if cap.is_null() || secret.is_null() {
        return CCapResult::NullRequiredArgs;
    }

    let rust_cap = match $rtype::from_bits((*cap).signature.into(), (*cap).data.into()) {
        Ok(cap) => cap,
        Err(err) => return err.into()
    };
    match rust_cap.check_validity((*secret).into()) {
        Ok(_) => CCapResult::Success,
        Err(err) => err.into(),
    }
}

/// Given a pointer to a capability, read off its base and length.
/// len_64 will be set if the range.len() has the 64th bit set.
/// base, len, and len_64 are optional arguments, and are ignored if null.
/// cap is non-optional, and the function returns `NullRequiredArgs` if null.
/// Returns a Decode error if the capability data is invalid.
/// Doesn't check the capability signature.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _read_range >](cap: *const $ctype, base: *mut u64, len: *mut u64, len_64: *mut bool) -> CCapResult {
    if cap.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    let cap_data: u128 = (*cap).data.into();
    let cap_data: <$rtype as Cap2024>::CapData = match cap_data.try_into() {
        Ok(cap_data) => cap_data,
        Err(e) => return e.into(),
    };
    let range = match cap_data.encoded_range() {
        Ok(range) => range,
        Err(e) => return e.into()
    };
    if !base.is_null() {
        *base = range.base()
    }
    if !len.is_null() {
        *len = range.len() as u64
    }
    if !len_64.is_null() {
        *len_64 = (range.len() >> 64) != 0;
    }
    CCapResult::Success
}

/// Given a pointer to a capability, read off it's permissions (Read, Write, or both).
/// cap and perms are non-optional, and the function returns `NullRequiredArgs` if they're null.
/// Returns a Decode error if the capability permissions chain is invalid, but does not check any other part of the capability.
/// Doesn't check the capability signature.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _read_perms >](cap: *const $ctype, perms: *mut CCapPerms) -> CCapResult {
    if cap.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    let cap_data: u128 = (*cap).data.into();
    let cap_perms_chain: CapPermsChain = match <$rtype as Cap2024>::cap_perms_bitfield_from_raw(cap_data) {
        Ok(cap_perms_chain) => cap_perms_chain,
        Err(e) => return e.into(),
    };
    if perms.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    let cap_perms: CapPerms = cap_perms_chain.into();
    *perms = cap_perms.into();
    CCapResult::Success
}

/// Given a pointer to a capability, read off the secret-key id it claims to use.
/// cap and secret_id are non-optional, and the function returns `NullRequiredArgs` if they're null.
/// Returns a Decode error if the capability data is invalid.
/// Doesn't check the capability signature.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _read_secret_id >](cap: *const $ctype, secret_id: *mut u32) -> CCapResult {
    if cap.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    let cap_data: u128 = (*cap).data.into();
    let cap_data: <$rtype as Cap2024>::CapData = match cap_data.try_into() {
        Ok(cap_data) => cap_data,
        Err(e) => return e.into(),
    };
    if secret_id.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    *secret_id = cap_data.secret_key_id;
    CCapResult::Success
}

/// Initialize a capability from scratch, given the contiguous memory range it grants access to, the permissions (Read|Write), and the secret_id.
/// Uses the initial resource and both caveats if necessary.
/// Calculates the capability signature given the packed data and the secret.
///
/// The memory range, permissions, and some parts of the secret_id are extracted from the virtio_desc.
///
/// The addr and len fields of virtio_desc dictate the base and len of the capability respectively.
///
/// The INDIRECT and NEXT virtio flags are packed into the top two bits of the secret_id.
/// The `next` field of the virtio descriptor is packed into the next thirteen bits of the secret_id,
/// leaving only 8 bits for the actual secret_id.
///
/// |- INDIRECT -|- NEXT -|- next[12:0] -|- key[7:0] -|
///      [22]       [21]       [20:8]         [7:0]
///
/// The WRITE virtio flag determines the capability permissions: if it is set, the permissions are `CCapPerms::Write`, else `CCapPerms::Read`.
///
/// cap, secret, and virtio_desc are non-optional, and the function returns `NullRequiredArgs` if any are null.
///
/// Returns an Encode error if the base/length is not exactly representable.
/// Use [ccap$version_init_virtio_cavs_inexact] to allow rounding the bounds up in this case instead of returning an error.
///
/// Returns `Encode_TooBigSecretId` if `virtio_desc.next` does not fit into 13 bits.
///
/// Uses caveats
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _init_virtio_cavs_exact >](cap: *mut $ctype, secret: *const CCapU128, virtio_desc: *const CCapNativeVirtqDesc, secret_id: u8) -> CCapResult {
    if cap.is_null() || secret.is_null() || virtio_desc.is_null() {
        return CCapResult::NullRequiredArgs;
    }

    let perms = if ((*virtio_desc).flags & CCAP_VIRTQ_F_WRITE) != 0 { CCapPerms::Write } else { CCapPerms::Read };

    if (*virtio_desc).next >= (1 << 13) {
        return CCapResult::Encode_TooBigSecretId;
    }

    // Construct the new secret_id from parts
    let secret_id: u32 =
         if ((*virtio_desc).flags & CCAP_VIRTQ_F_INDIRECT) != 0 { 1 << 22 } else { 0 }
         | if ((*virtio_desc).flags & CCAP_VIRTQ_F_NEXT) != 0 { 1 << 21 } else { 0 }
         | ((*virtio_desc).next as u32) << 8
         | (secret_id as u32) << 0;

    [< $prefix _init_cavs_exact >](cap, secret, (*virtio_desc).addr, (*virtio_desc).len as u64, secret_id, perms)
}

/// Given a pointer to a capability, extract all data into a virtio-esque descriptor.
///
/// The INDIRECT and NEXT virtio flags are packed into the top two bits of the secret_id.
/// The `next` field of the virtio descriptor is packed into the next thirteen bits of the secret_id,
/// leaving only 8 bits for the actual secret_id.
///
/// |- INDIRECT -|- NEXT -|- next[12:0] -|- key[7:0] -|
///      [22]       [21]       [20:8]         [7:0]
///
/// The WRITE virtio flag determines the capability permissions: if it is set, the permissions are `CCapPerms::Write`, else `CCapPerms::Read`.
///
/// cap and virtio_desc are non-optional, and the function returns `NullRequiredArgs` if they're null.
///
/// Returns a Decode error if the capability data is invalid.
///
/// Additionally, returns `Decode_NotVirtio` if
/// - the permissions allow both read and write
/// - the length does not fit into a u32
///
/// In practice this means you should only call this function on capabilities encoded through [$ccap_version_init_virtio_exact], where those invariants are enforced.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _read_virtio >](cap: *const $ctype, virtio_desc: *mut CCapNativeVirtqDesc) -> CCapResult {
    if cap.is_null() || virtio_desc.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    let cap_data: u128 = (*cap).data.into();
    let cap_data: <$rtype as Cap2024>::CapData = match cap_data.try_into() {
        Ok(cap_data) => cap_data,
        Err(e) => return e.into(),
    };
    let range = match cap_data.encoded_range() {
        Ok(range) => range,
        Err(e) => return e.into()
    };

    if virtio_desc.is_null() {
        return CCapResult::NullRequiredArgs;
    }

    let perms: CapPerms = cap_data.perms_chain.into();
    if (perms == CapPerms::ReadWrite) {
        return CCapResult::Decode_NotVirtio;
    }

    let addr: u64 = range.base();
    let len: u32 = match range.len().try_into() {
        Ok(len_32) => len_32,
        Err(_) => return CCapResult::Decode_NotVirtio,
    };
    let flags: u16 =
        if ((cap_data.secret_key_id >> 22) & 1) != 0 { CCAP_VIRTQ_F_INDIRECT } else { 0 }
        | if ((cap_data.secret_key_id >> 21) & 1) != 0 { CCAP_VIRTQ_F_NEXT } else { 0 }
        | if (perms == CapPerms::Write) { CCAP_VIRTQ_F_WRITE } else { 0 };
    let next: u16 = ((cap_data.secret_key_id >> 8) & 0x1FFF) as u16;

    *virtio_desc = CCapNativeVirtqDesc {
        addr,
        len,
        flags,
        next,
    };

    CCapResult::Success
}

/* TODO GET THIS STUFF TO WORK WITH MANUAL BITFIELD TWIDDLING LIKE C

/// Given a pointer to a capaility initialized with ccap2024_11_init_virtio_*,
/// read out the encoded 'next' field.
/// Can never fail.
/// DOES NOT CHECK IF cap IS NULL.
///
/// |- INDIRECT -|- NEXT -|- next[12:0] -|- key[7:0] -|
///      [22]       [21]       [20:8]         [7:0]
///
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _read_virtio_next >](cap: *const $ctype) -> u16 {
    let cap_data: u128 = (*cap).data.into();
    let cap_data: <$rtype as Cap2024>::CapData = match cap_data.try_into() {
        Ok(cap_data) => cap_data,
        Err(e) => return e.into(),
    };

    let next: u16 = ((cap_data.secret_key_id >> 8) & 0x1FFF) as u16;

    next
}

/// Given a pointer to a capability initialized with ccap2024_11_init_virtio_*,
/// read out the INDIRECT and NEXT flags.
/// Can never fail. Does not read out the WRITE flag because that is encoded in the permissions chain,
/// which has one invalid value and thus can fail.
/// DOES NOT CHECK IF cap IS NULL.
///
/// |- INDIRECT -|- NEXT -|- next[12:0] -|- key[7:0] -|
///      [22]       [21]       [20:8]         [7:0]
pub unsafe extern "C" fn [< $prefix _read_virtio_flags_indirect_next >](cap: *const $ctype) -> u16 {
    let cap_data: u128 = (*cap).data.into();
    let cap_data: <$rtype as Cap2024>::CapData = match cap_data.try_into() {
        Ok(cap_data) => cap_data,
        Err(e) => return e.into(),
    };

    let flags: u16 =
        if ((cap_data.secret_key_id >> 22) & 1) != 0 { CCAP_VIRTQ_F_INDIRECT } else { 0 }
        | if ((cap_data.secret_key_id >> 21) & 1) != 0 { CCAP_VIRTQ_F_NEXT } else { 0 };

    flags
}

/// Given a pointer to a capability, clear the data (not necessarily with 0s, but to an unusable state)
/// and overwrite the data that would encode the 'next' field with the given value.
/// This is useful for systems like FreeBSD which use this field in normal descriptors to store information.
///
/// cap is non-optional, and the function returns `NullRequiredArgs` if they're null.
///
/// Returns `Encode_TooBigSecretId` if `next` does not fit into 13 bits.
pub unsafe extern "C" fn [< $prefix _clear_and_write_virtio_next >](cap: *mut $ctype, next: u16) -> CCapResult {
    if cap.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    if next >= (1 << 13) {
        return CCapResult::Encode_TooBigSecretId;
    }

    let rust_cap = $rtype::new(
        0,
        ((next as u32) << 8),
        0,
        1,
        CCapPerms::Read.into()
    );
    match rust_cap {
        Ok(rust_cap) => {
            *cap = rust_cap.into();
            CCapResult::Success
        }
        Err(err) => panic!("{}", err),
    }
}
    */

        }
    };
}

use rand::SeedableRng;
use rust_caps::capability::v2024::rand::RandCap;

fn rng_from_seed(seed_in: &CCapU128) -> rand::rngs::StdRng {
    // Seed = concat (seed_in) ([0; 16])
    let mut seed = [0; 32];
    seed.split_at_mut(16).0.copy_from_slice(&seed_in.0);

    rand::rngs::StdRng::from_seed(seed)
}

/// Define the random generation API in a rust macro so we can reproduce it for multiple different capability format versions
macro_rules! ccap_rand_api {
    ($prefix:ident, $ctype:ty, $rtype:ty) => {
        paste! {
/// Randomly generates a valid capability (i.e. one with a valid signature, which decodes with no errors, encoding a nonzero address range) with 0, 1, or 2 caveats based on the seed.
///
/// cap and seed are non-optional, and the function returns `NullRequiredArgs` if they're null.
///
/// secret_key and secret_key_id may be null, in which case they will be randomly generated based on the seed.
/// perms may be null, in which case the permissions chain will be randomly selected based on the seed,
/// otherwise the permissions chain will be randomly selected from those that resolve to those perms.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _rand_valid_cap >](cap: *mut $ctype, seed: *const CCapU128, secret_key: *const CCapU128, secret_key_id: *const u32, perms: *const CCapPerms) -> CCapResult {
    if cap.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    if seed.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    let secret_key: Option<u128> = secret_key.as_ref().map(|secret_key_ccapu128| (*secret_key_ccapu128).into());
    let secret_key_id: Option<u32> = secret_key_id.as_ref().map(|secret_key_id: &u32| *secret_key_id);
    let perms: Option<CapPerms> = perms.as_ref().map(|perms| (*perms).into());

    let random = <$rtype as RandCap>::rand_cap(&mut rng_from_seed(&*seed), secret_key, secret_key_id, None, perms);

    (*cap).signature = random.sig.into();
    (*cap).data = random.data.into();

    CCapResult::Success
}

/// Randomly generates a valid capability (i.e. one with a valid signature, which decodes with no errors, encoding a nonzero address range) with 0 caveats based on the seed.
///
/// cap and seed are non-optional, and the function returns `NullRequiredArgs` if they're null.
///
/// secret_key and secret_key_id may be null, in which case they will be randomly generated based on the seed.
/// perms may be null, in which case the permissions chain will be randomly selected from 0-caveat options based on the seed,
/// otherwise the permissions chain will be randomly selected from 0-caveat options that resolve to those perms.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _rand_valid_cap_0cav >](cap: *mut $ctype, seed: *const CCapU128, secret_key: *const CCapU128, secret_key_id: *const u32, perms: *const CCapPerms) -> CCapResult {
    if cap.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    if seed.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    let secret_key: Option<u128> = secret_key.as_ref().map(|secret_key_ccapu128| (*secret_key_ccapu128).into());
    let secret_key_id: Option<u32> = secret_key_id.as_ref().map(|secret_key_id: &u32| *secret_key_id);
    let perms: Option<CapPerms> = perms.as_ref().map(|perms| (*perms).into());

    let random = <$rtype as RandCap>::rand_0cav_cap(&mut rng_from_seed(&*seed), secret_key, secret_key_id, None, perms);

    (*cap).signature = random.sig.into();
    (*cap).data = random.data.into();

    CCapResult::Success
}

/// Randomly generates a valid capability (i.e. one with a valid signature, which decodes with no errors, encoding a nonzero address range) with 1 caveat based on the seed.
///
/// cap and seed are non-optional, and the function returns `NullRequiredArgs` if they're null.
///
/// secret_key and secret_key_id may be null, in which case they will be randomly generated based on the seed.
/// perms may be null, in which case the permissions chain will be randomly selected from 1-caveat options based on the seed,
/// otherwise the permissions chain will be randomly selected from 1-caveat options that resolve to those perms.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _rand_valid_cap_1cav >](cap: *mut $ctype, seed: *const CCapU128, secret_key: *const CCapU128, secret_key_id: *const u32, perms: *const CCapPerms) -> CCapResult {
    if cap.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    if seed.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    let secret_key: Option<u128> = secret_key.as_ref().map(|secret_key_ccapu128| (*secret_key_ccapu128).into());
    let secret_key_id: Option<u32> = secret_key_id.as_ref().map(|secret_key_id: &u32| *secret_key_id);
    let perms: Option<CapPerms> = perms.as_ref().map(|perms| (*perms).into());

    let random = <$rtype as RandCap>::rand_1cav_cap(&mut rng_from_seed(&*seed), secret_key, secret_key_id, None, perms);

    (*cap).signature = random.sig.into();
    (*cap).data = random.data.into();

    CCapResult::Success
}

/// Randomly generates a valid capability (i.e. one with a valid signature, which decodes with no errors, encoding a nonzero address range) with 2 caveats based on the seed.
///
/// cap and seed are non-optional, and the function returns `NullRequiredArgs` if they're null.
///
/// secret_key and secret_key_id may be null, in which case they will be randomly generated based on the seed.
/// perms may be null, in which case the permissions chain will be randomly selected from 2-caveat options based on the seed,
/// otherwise the permissions chain will be randomly selected from 2-caveat options that resolve to those perms.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _rand_valid_cap_2cav >](cap: *mut $ctype, seed: *const CCapU128, secret_key: *const CCapU128, secret_key_id: *const u32, perms: *const CCapPerms) -> CCapResult {
    if cap.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    if seed.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    let secret_key: Option<u128> = secret_key.as_ref().map(|secret_key_ccapu128| (*secret_key_ccapu128).into());
    let secret_key_id: Option<u32> = secret_key_id.as_ref().map(|secret_key_id: &u32| *secret_key_id);
    let perms: Option<CapPerms> = perms.as_ref().map(|perms| (*perms).into());

    let random = <$rtype as RandCap>::rand_2cav_cap(&mut rng_from_seed(&*seed), secret_key, secret_key_id, None, perms);

    (*cap).signature = random.sig.into();
    (*cap).data = random.data.into();

    CCapResult::Success
}

/// Randomly generates a capability using one of the "edge case" generators.
/// The number of edge cases for ccap$version is returned by [ccap$version_rand_edge_case_num].
/// Panics if the supplied edge case number >= [ccap$version_rand_edge_case_num].
///
/// cap and seed are non-optional, and the function returns `NullRequiredArgs` if they're null.
///
/// secret_key and secret_key_id may be null, in which case they will be randomly generated based on the seed.
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _rand_edge_case_cap >](cap: *mut $ctype, seed: *const CCapU128, secret_key: *const CCapU128, secret_key_id: *const u32, edge_case: usize) -> CCapResult {
    if cap.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    if seed.is_null() {
        return CCapResult::NullRequiredArgs;
    }
    let secret_key: Option<u128> = secret_key.as_ref().map(|secret_key_ccapu128| (*secret_key_ccapu128).into());
    let secret_key_id: Option<u32> = secret_key_id.as_ref().map(|secret_key_id: &u32| *secret_key_id);

    let edge_case = <$rtype as RandCap>::enum_edge_cases()[edge_case];
    let random = <$rtype as RandCap>::rand_edge_case(&mut rng_from_seed(&*seed), secret_key, secret_key_id, edge_case);

    (*cap).signature = random.sig.into();
    (*cap).data = random.data.into();

    CCapResult::Success
}

/// Returns the number of "edge case" random generators created for ccap$version
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _rand_edge_case_num >]() -> usize {
    <$rtype as RandCap>::enum_edge_cases().len()
}

/// Converts an edge case number smaller than [ccap$version_rand_edge_case_num] to a string describing the specific edge case.
///
/// Panics if the number is greater than or equal to the [ccap$version_rand_edge_case_num]
#[no_mangle]
pub unsafe extern "C" fn [< $prefix _rand_edge_case_str >](edge_case: usize) -> *const core::ffi::c_char {
    let cstr: &'static core::ffi::CStr = <$rtype as RandCap>::enum_edge_cases()[edge_case].into();
    cstr.as_ptr()
}

        }
    }
}

ccap_api!(ccap2024_02, CCap2024_02, Cap2024_02::<Aes128HandrolledSig>);
ccap_rand_api!(ccap2024_02, CCap2024_02, Cap2024_02::<Aes128HandrolledSig>);
ccap_api!(ccap2024_11, CCap2024_11, Cap2024_11::<Aes128HandrolledSig>);
ccap_rand_api!(ccap2024_11, CCap2024_11, Cap2024_11::<Aes128HandrolledSig>);

fn internal_init_cavs<Cap: Cap2024>(
    secret: u128,
    base: u64,
    len: u64,
    secret_id: u32,
    perms: CCapPerms,
) -> (Option<Cap>, CCapResult) {
    let cap = match Cap::new_inexact(secret, secret_id, base, len.into(), perms.into()) {
        Ok(rust_cap) => rust_cap,
        Err(e) => return (None, e.into()),
    };

    let (initial_range, _) = cap.requested_range().unwrap();
    let target_range = AddrRange::base_len_64(base, len);
    if !initial_range.encompasses(&target_range) {
        panic!("initial_range {initial_range} does not encompass target {target_range}");
    }

    if initial_range == target_range {
        (Some(cap), CCapResult::Success)
    } else {
        let cap_with_cav1 = match cap.refine_using_cav1_towards(None, target_range) {
            Ok(ok) => ok,
            Err(e) => panic!("Encountered err {e:?} while shrinking capability {cap:x?}"),
        };
        let (cav1_range, _) = cap_with_cav1.requested_range().unwrap();

        if !cav1_range.encompasses(&target_range) {
            panic!("cav1_range {cav1_range} does not encompass target {target_range}");
        }

        if cav1_range == target_range {
            (Some(cap_with_cav1), CCapResult::Success)
        } else {
            let cap_with_cav2 = cap_with_cav1
                .refine_using_cav2_towards(None, target_range)
                .unwrap();
            let (cav2_range, _) = cap_with_cav2.requested_range().unwrap();

            if !cav2_range.encompasses(&target_range) {
                panic!("cav2_range {cav2_range} does not encompass target {target_range}");
            }

            if cav2_range == target_range {
                (Some(cap_with_cav2), CCapResult::Success)
            } else {
                (
                    Some(cap_with_cav2),
                    CCapResult::Encode_UnrepresentableCaveat,
                )
            }
        }
    }
}

#[cfg(test)]
mod test {
    use rust_caps::{
        capability::{v2024::Cap2024, v2024_02::Cap2024_02, v2024_11::Cap2024_11},
        crypto::NullSig,
    };

    use crate::{internal_init_cavs, CCapResult};

    fn print_cap<Cap: Cap2024>(base: u64, len: u64) {
        match internal_init_cavs::<Cap>(0, base, len, 0, crate::CCapPerms::ReadWrite).0 {
            None => panic!("Got None for 0x{base:x} 0x{len:x}"),
            Some(cap) => println!(
                "Some({:x?} for {:x?})",
                cap.cap_data(),
                cap.requested_range().unwrap()
            ),
        }
    }

    fn test_cap_exact<Cap: Cap2024>(base: u64, len: u64) {
        match internal_init_cavs::<Cap>(0, base, len, 0, crate::CCapPerms::ReadWrite) {
            (None, _) => panic!("Got None for 0x{base:x} 0x{len:x}"),
            (Some(cap), res) => {
                let range = cap.requested_range().unwrap().0;
                if range.base() != base || range.len() != len.into() || res != CCapResult::Success {
                    panic!("Tried to encode base 0x{base:x} and len 0x{len:x} but couldn't get it exact: got 0x{:x} len 0x{:x}, res = {res:?}", range.base(), range.len());
                }
            }
        }
    }

    fn specific_ranges<Cap: Cap2024>() {
        test_cap_exact::<Cap>(0xc8425c80, 0x10);
        test_cap_exact::<Cap>(0x00000000c2029440, 0x200);
        test_cap_exact::<Cap>(0x00000000c2004fd7, 0x01);
        test_cap_exact::<Cap>(0x00000000c2029640, 0x200);
        test_cap_exact::<Cap>(0x00000000c2251e8c, 0x5ea);
    }

    #[test]
    fn specific_ranges_2024_02() {
        specific_ranges::<Cap2024_02<NullSig>>();
    }
    #[test]
    fn specific_ranges_2024_11() {
        specific_ranges::<Cap2024_11<NullSig>>();
    }

    // 2024_02
    // Any region <= 2^10 (which implicitly will be represented by at largest a 2^11 cav1)
    // should be exactly representable using two caveats (cav2 is 2^9 reduction => will always be able to shrink a 2^11 cav1 down to 2^2)
    #[test]
    fn exhaustive_4_aligned_under_kib_with_2cav_2024_02() {
        println!();
        const MAX: u64 = 1 << 10;
        for base in (0..=MAX).step_by(4) {
            for len in (0..=MAX).step_by(4) {
                if base == 0 || len == 0 {
                    continue;
                }
                println!("base: 0x{base:04x} len: 0x{len:04x}");
                test_cap_exact::<Cap2024_02<NullSig>>(base, len)
            }
        }
    }

    // 2024_11
    // Any region <= 2^13 (which implicitly will be represented by at largest a 2^14 cav1, if it overlaps a 2^13 boundary)
    // should be exactly representable using two caveats (cav2 is 2^14 reduction => will always be able to shrink a 2^14 cav1 down to 2^0)
    // Another way to write this is that any region inside a 2^14 aligned block (which will be represented by at largest a 2^14 cav1) can be shrunk down to an arbitrary size
    #[test]
    fn exhaustive_1_aligned_under_8kib_with_2cav_2024_11() {
        println!();
        for base in 0..=(1 << 13) {
            for len in 0..=(1 << 13) {
                if len == 0 {
                    continue;
                }
                println!("base: 0x{base:04x} len: 0x{len:04x}");
                test_cap_exact::<Cap2024_11<NullSig>>(base, len)
            }
        }
    }

    #[test]
    fn exhaustive_1_aligned_inside_16kib_with_2cav_2024_11() {
        println!();
        for base in 0..(1 << 14) {
            for len in 1..=((1 << 14) - base) {
                println!("base: 0x{base:04x} len: 0x{len:04x}");
                test_cap_exact::<Cap2024_11<NullSig>>(base, len)
            }
        }
    }

    fn exhaustive_1_not_always_exact_but_never_crashes<Cap: Cap2024>() {
        const SECRET: u128 = 0;
        const SECRET_ID: u32 = 0;
        const PERMS: crate::CCapPerms = crate::CCapPerms::ReadWrite;
        const BASE: u64 = (1 << 10) - 1;
        for len in 1..=(1 << 10) {
            dbg!(BASE, len);
            internal_init_cavs::<Cap>(SECRET, BASE, len, SECRET_ID, PERMS);
        }
    }

    #[test]
    fn exhaustive_1_not_always_exact_but_never_crashes_2024_02() {
        exhaustive_1_not_always_exact_but_never_crashes::<Cap2024_02<NullSig>>();
    }
    #[test]
    fn exhaustive_1_not_always_exact_but_never_crashes_2024_11() {
        exhaustive_1_not_always_exact_but_never_crashes::<Cap2024_11<NullSig>>();
    }
}

// In standalone environments we need a way to handle panics.
// Because we will be linking these libraries into various places,
// it's more sensible to give them the option to define the print function.

extern "C" {
    /// A function called when librust_caps_c panics to print debug information.
    ///
    /// This function is *not* defined by librust_caps_c, and must be defined by the target linking the library in.
    #[cfg(not(feature = "std"))]
    pub fn ccap_panic_write_utf8(utf8: *const u8, utf_len: u64) -> u64;

    /// A function called once librust_caps_c has finished printing debug information,
    /// allowing the error to be surfaced to the relevant systems via e.g. an exception or an abort.
    ///
    /// This function is *not* defined by librust_caps_c, and must be defined by the target linking the library in.
    #[cfg(not(feature = "std"))]
    pub fn ccap_panic_complete();
}

/// core::fmt::Write impl which calls ccap_panic_write_utf8().
#[cfg(not(feature = "std"))]
struct PanicWriter();
#[cfg(not(feature = "std"))]
use core::fmt::Write;
#[cfg(not(feature = "std"))]
impl Write for PanicWriter {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        let expected_len = s.len() as u64;
        let actual_len = unsafe { ccap_panic_write_utf8(s.as_bytes().as_ptr(), expected_len) };
        if expected_len != actual_len {
            Err(core::fmt::Error)
        } else {
            Ok(())
        }
    }
}

/// The actual panic handler.
#[cfg(not(feature = "std"))]
#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    // logs "panicked at '$reason', src/main.rs:27:4" to the host stderr
    writeln!(PanicWriter(), "{}", info).ok();

    unsafe {
        ccap_panic_complete();
    }

    loop {}
}
