use super::{AddrRange, CapDecodeErr, CapEncodeErr, CapPerms, Capability};

pub mod perms;
use perms::*;
pub mod rand;

pub trait Cap2024: Sized + Capability + core::fmt::Debug + Clone {
    type CapData: core::fmt::Debug;

    fn cap_data(&self) -> &Self::CapData;
    fn cap_perms_chain(&self) -> CapPermsChain;
    fn cap_perms(&self) -> CapPerms;
    fn secret_key_id(&self) -> u32;
    fn new(secret: u128, secret_key_id: u32, base: u64, len: u128, perms: CapPerms) -> Result<Self, CapEncodeErr>;
    fn new_inexact(secret: u128, secret_key_id: u32, base: u64, len: u128, perms: CapPerms) -> Result<Self, CapEncodeErr>;
    fn from_bits(sig: u128, data: u128) -> Result<Self, CapDecodeErr>;
    fn refine_using_cav1_towards(&self, new_perms: Option<CapPerms>, target: AddrRange) -> Result<Self, CapEncodeErr>;
    fn with_cav1(&self, new_perms: Option<CapPerms>, index: u16, pow2: u8) -> Result<Self, CapEncodeErr>;
    fn with_cav1_size_div(&self, new_perms: Option<CapPerms>, index: u16, size_div: u8) -> Result<Self, CapEncodeErr>;
    fn refine_using_cav2_towards(&self, new_perms: Option<CapPerms>, target: AddrRange) -> Result<Self, CapEncodeErr>;
    fn with_cav2(&self, new_perms: Option<CapPerms>, range_x: u16, range_y: u16) -> Result<Self, CapEncodeErr>;
    fn cap_perms_bitfield_from_raw(data: u128) -> Result<CapPermsChain, CapDecodeErr>;
}
