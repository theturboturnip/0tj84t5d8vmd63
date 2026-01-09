use core::ops::{BitOr, Shl, Shr};

use arrayvec::ArrayVec;
use rand::{
    distributions::{uniform::SampleUniform, Distribution, Standard},
    Rng,
};

use crate::{
    capability::{AddrRange, CapDecodeErr, CapPerms},
    util::align_to_exact,
};

use super::{
    CapPermsChain, ALL_0CAV_CAP_PERMS_CHAINS, ALL_1CAV_CAP_PERMS_CHAINS, ALL_2CAV_CAP_PERMS_CHAINS,
    ALL_VALID_CAP_PERMS_CHAINS,
};

#[derive(Debug, Clone, Copy)]
pub enum ChooseRand<T>
where
    T: core::fmt::Debug
        + SampleUniform
        + Shl<usize, Output = T>
        + Shr<usize, Output = T>
        + BitOr<Output = T>
        + From<bool>
        + core::cmp::PartialOrd,
    Standard: Distribution<T>,
{
    Any,
    AnyNBits(usize),
    AnyAligned(usize),
    Specific(T),
    /// Uniform distribution sampling over inclusive range (.0)..=(.1)
    Between(T, T),
}
impl<T> ChooseRand<T>
where
    T: core::fmt::Debug
        + SampleUniform
        + Shl<usize, Output = T>
        + Shr<usize, Output = T>
        + BitOr<Output = T>
        + From<bool>
        + core::cmp::PartialOrd,
    Standard: Distribution<T>,
{
    pub fn sample<R: Rng>(self, rng: &mut R) -> T {
        match self {
            ChooseRand::Any => rng.gen(),
            ChooseRand::AnyNBits(n) => {
                let zero: T = false.into();
                let one: T = true.into();
                rng.gen_range(zero..(one << n))
            }
            ChooseRand::AnyAligned(to) => align_to_exact(rng.gen(), to),
            ChooseRand::Specific(t) => t,
            ChooseRand::Between(a, b) => {
                if a > b {
                    panic!("Cannot ChooseRand::Between({:?}, {:?}) where a > b", a, b);
                } else if a == b {
                    a
                } else {
                    rng.gen_range(a..=b)
                }
            }
        }
    }
}

pub fn choose_from<T: Copy, R: Rng>(rng: &mut R, array: &[T]) -> T {
    array[rng.gen_range(0..array.len())]
}

pub struct CapTestCase {
    pub secret: u128,
    pub sig: u128,
    pub data: u128,
    pub expected: Result<(AddrRange, CapPerms), CapDecodeErr>,
}

fn choose_filtered_chain<const N: usize, R: Rng>(
    rng: &mut R,
    chains: &[CapPermsChain; N],
    perms: Option<CapPerms>,
) -> CapPermsChain {
    match perms {
        None => choose_from(rng, chains),
        Some(specific_perm) => {
            let base_set = chains;
            let mut refined_opts = ArrayVec::<CapPermsChain, N>::new();
            for chain in base_set {
                if specific_perm == (*chain).into() {
                    unsafe {
                        refined_opts.push_unchecked(*chain);
                    }
                }
            }
            choose_from(rng, refined_opts.as_slice())
        }
    }
}

pub trait RandCap {
    /// The range of values that can be passed into rand_XXX_cap as initial_len_pow2.
    const INITIAL_LEN_POW2_RANGE: core::ops::RangeInclusive<u8>;

    /// Generate a random valid capability with a specific chain of permissions.
    /// The chain of permissions encodes both the number of caveats and the final [CapPerms].
    fn rand_valid_cap_with_perms_chain<R: Rng>(
        rng: &mut R,
        secret: Option<u128>,
        secret_key_id: Option<u32>,
        initial_len_pow2: Option<u8>,
        chain: CapPermsChain,
    ) -> CapTestCase;

    /// Specialization of [RandCap::rand_valid_cap_with_perms_chain] that randomly selects a 0-cav permissions chain,
    /// optionally with the specified permissions.
    fn rand_0cav_cap<R: Rng>(
        rng: &mut R,
        secret: Option<u128>,
        secret_key_id: Option<u32>,
        initial_len_pow2: Option<u8>,
        perms: Option<CapPerms>,
    ) -> CapTestCase {
        let chain = choose_filtered_chain(rng, &ALL_0CAV_CAP_PERMS_CHAINS, perms);
        Self::rand_valid_cap_with_perms_chain(rng, secret, secret_key_id, initial_len_pow2, chain)
    }

    /// Specialization of [RandCap::rand_valid_cap_with_perms_chain] that randomly selects a 1-cav permissions chain,
    /// optionally with the specified permissions.
    fn rand_1cav_cap<R: Rng>(
        rng: &mut R,
        secret: Option<u128>,
        secret_key_id: Option<u32>,
        initial_len_pow2: Option<u8>,
        perms: Option<CapPerms>,
    ) -> CapTestCase {
        let chain = choose_filtered_chain(rng, &ALL_1CAV_CAP_PERMS_CHAINS, perms);
        Self::rand_valid_cap_with_perms_chain(rng, secret, secret_key_id, initial_len_pow2, chain)
    }

    /// Specialization of [RandCap::rand_valid_cap_with_perms_chain] that randomly selects a 2-cav permissions chain,
    /// optionally with the specified permissions.
    fn rand_2cav_cap<R: Rng>(
        rng: &mut R,
        secret: Option<u128>,
        secret_key_id: Option<u32>,
        initial_len_pow2: Option<u8>,
        perms: Option<CapPerms>,
    ) -> CapTestCase {
        let chain = choose_filtered_chain(rng, &ALL_2CAV_CAP_PERMS_CHAINS, perms);
        Self::rand_valid_cap_with_perms_chain(rng, secret, secret_key_id, initial_len_pow2, chain)
    }

    /// Specialization of [RandCap::rand_valid_cap_with_perms_chain] that randomly selects any valid permissions chain
    fn rand_cap<R: Rng>(
        rng: &mut R,
        secret: Option<u128>,
        secret_key_id: Option<u32>,
        initial_len_pow2: Option<u8>,
        perms: Option<CapPerms>,
    ) -> CapTestCase {
        let chain = choose_filtered_chain(rng, &ALL_VALID_CAP_PERMS_CHAINS, perms);
        Self::rand_valid_cap_with_perms_chain(rng, secret, secret_key_id, initial_len_pow2, chain)
    }

    /// Enum denoting a specific edge case
    type EdgeCase: Sized + Into<&'static core::ffi::CStr> + core::fmt::Debug + Clone + Copy;
    /// Retrieve all possible edge cases
    fn enum_edge_cases() -> &'static [Self::EdgeCase];
    /// Generate a random valid capability under a specific edge case
    fn rand_edge_case<R: Rng>(
        rng: &mut R,
        secret: Option<u128>,
        secret_key_id: Option<u32>,
        edge_case: Self::EdgeCase,
    ) -> CapTestCase;
}
