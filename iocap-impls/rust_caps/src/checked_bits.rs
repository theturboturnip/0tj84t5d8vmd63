use core::{marker::PhantomData, ops::RangeInclusive};

pub trait BitNumBacking: Sized +  
    core::fmt::Debug +
    core::fmt::LowerHex + 
    core::ops::BitAnd<Output=Self> + 
    core::ops::BitOr<Output=Self> + 
    core::ops::BitXor<Output=Self> + 
    core::ops::Not<Output=Self> + 
    core::ops::Shl<usize, Output=Self> + 
    core::ops::Shr<usize, Output=Self> + 
    core::cmp::PartialEq + 
    core::cmp::PartialOrd +
    core::cmp::Ord +
    Copy + Clone {
    const BITS: usize;
    const ONE: Self;
    const ZERO: Self;
    const BITNUM_MAX: Self;

    fn checked_add(self, rhs: Self) -> Option<Self>;
    fn checked_sub(self, rhs: Self) -> Option<Self>;
    fn checked_ilog2(self) -> Option<u32>;
    fn leading_zeros(self) -> usize;
}
impl BitNumBacking for u8 {
    const BITS: usize = 8;
    const ONE: Self = 1;
    const ZERO: Self = 0;
    const BITNUM_MAX: Self = u8::MAX;

    fn checked_add(self, rhs: Self) -> Option<Self> {
        self.checked_add(rhs)
    }
    fn checked_sub(self, rhs: Self) -> Option<Self> {
        self.checked_sub(rhs)
    }
    fn checked_ilog2(self) -> Option<u32> {
        self.checked_ilog2()
    }
    fn leading_zeros(self) -> usize {
        self.leading_zeros() as usize
    }
}
impl BitNumBacking for u16 {
    const BITS: usize = 16;
    const ONE: Self = 1;
    const ZERO: Self = 0;
    const BITNUM_MAX: Self = u16::MAX;

    fn checked_add(self, rhs: Self) -> Option<Self> {
        self.checked_add(rhs)
    }
    fn checked_sub(self, rhs: Self) -> Option<Self> {
        self.checked_sub(rhs)
    }
    fn checked_ilog2(self) -> Option<u32> {
        self.checked_ilog2()
    }
    fn leading_zeros(self) -> usize {
        self.leading_zeros() as usize
    }
}
impl BitNumBacking for u32 {
    const BITS: usize = 32;
    const ONE: Self = 1;
    const ZERO: Self = 0;
    const BITNUM_MAX: Self = u32::MAX;

    fn checked_add(self, rhs: Self) -> Option<Self> {
        self.checked_add(rhs)
    }
    fn checked_sub(self, rhs: Self) -> Option<Self> {
        self.checked_sub(rhs)
    }
    fn checked_ilog2(self) -> Option<u32> {
        self.checked_ilog2()
    }
    fn leading_zeros(self) -> usize {
        self.leading_zeros() as usize
    }
}
impl BitNumBacking for u64 {
    const BITS: usize = 64;
    const ONE: Self = 1;
    const ZERO: Self = 0;
    const BITNUM_MAX: Self = u64::MAX;

    fn checked_add(self, rhs: Self) -> Option<Self> {
        self.checked_add(rhs)
    }
    fn checked_sub(self, rhs: Self) -> Option<Self> {
        self.checked_sub(rhs)
    }
    fn checked_ilog2(self) -> Option<u32> {
        self.checked_ilog2()
    }
    fn leading_zeros(self) -> usize {
        self.leading_zeros() as usize
    }
}
impl BitNumBacking for u128 {
    const BITS: usize = 128;
    const ONE: Self = 1;
    const ZERO: Self = 0;
    const BITNUM_MAX: Self = u128::MAX;

    fn checked_add(self, rhs: Self) -> Option<Self> {
        self.checked_add(rhs)
    }
    fn checked_sub(self, rhs: Self) -> Option<Self> {
        self.checked_sub(rhs)
    }
    fn checked_ilog2(self) -> Option<u32> {
        self.checked_ilog2()
    }
    fn leading_zeros(self) -> usize {
        self.leading_zeros() as usize
    }
}

/// Helper type used to do compile-time assertions with const-generic parameters.
/// You can't do `const test = assert!(A < B);` in a function where `A` or `B` are generic parameters on the function,
/// because Rust says there may only be one definition of `test` for all monomorphized versions of the function.
/// Instead, `let _ = ConstCompare::<A, B>::LESS_THAN;` to dynamically post-monomorphization do a const assert using A and B.
pub struct ConstCompare<const A: usize, const B: usize>;
#[allow(dead_code)]
impl<const A: usize, const B: usize> ConstCompare<A, B> {
    pub const GREATER_THAN: () = assert!(A > B);
    pub const LESS_THAN: () = assert!(A < B);
    pub const EQ: () = assert!(A == B);
    pub const GREATER_THAN_EQ: () = assert!(A >= B);
    pub const LESS_THAN_EQ: () = assert!(A <= B);
    pub const A_EQ_B_DIV_4: () = assert!(A == (B / 4));
}


/// A bounded unsigned integer of value [BitInt::val] between a [BitInt::min] and [BitInt::max] (inclusive), which is at most N bits long in twos-complement representation.
///
/// Generic on the number of bits N and the "backing" T, which has to be large enough to fit N bits.
///
/// ```
/// # use rust_caps::checked_bits::BitInt;
/// // Declare a 5-bit unsigned integer with constant value of 0 (thus min and max are 0)
/// let x = BitInt::<u8, 5>::constant(0);
/// assert_eq!(x.min(), 0);
/// assert_eq!(x.val(), 0);
/// assert_eq!(x.max(), 0);
/// // Declare a 5-bit unsigned integer with an initial value of 20, which is implicitly bounded between 0 and 2^5 - 1 = 31
/// let y = BitInt::<u8, 5>::new(20);
/// assert_eq!(y.min(), 0);
/// assert_eq!(y.val(), 20);
/// assert_eq!(y.max(), 31);
/// // Declare a 5-bit unsigned integer with an initial value of 20 which is bounded between 5 and 21
/// let z = BitInt::<u8, 5>::new_bound(20, 5..=21);
/// assert_eq!(z.min(), 5);
/// assert_eq!(z.val(), 20);
/// assert_eq!(z.max(), 21);
/// ```
///
/// It's impossible to declare an integer with an N that doesn't fit in the backing: 
/// ```compile_fail
/// # use rust_caps::checked_bits::BitInt;
/// let x = BitInt::<u8, 75>::new(0);
/// ```
///
/// It's also impossible to declare a zero-size integer.
/// ```compile_fail
/// # use rust_caps::checked_bits::BitInt;
/// let x = BitInt::<u8, 0>::new(0);
/// ```
#[derive(Debug, Clone, Copy)]
pub struct BitInt<T: BitNumBacking, const N: usize> {
    val: T,
    min: T, // inclusive
    max: T, // inclusive
}
impl<T: BitNumBacking, const N: usize> BitInt<T, N> {
    #[allow(dead_code)]
    const CHECK: () = assert!(T::BITS >= N && N > 0);

    /// Get a bitmask where the first N bits are set and the rest are not.
    /// 
    /// I wish this could be const, but shift-right isn't const in Rust - checked_shr is, but that returns Option and Option::unwrap() etc are not const.
    ///
    /// ```
    /// # use rust_caps::checked_bits::BitInt;
    /// assert_eq!(BitInt::<u8, 5>::toplevel_mask(), 0b0001_1111)
    /// ```
    #[inline(always)]
    pub fn toplevel_mask() -> T {
        let _ = Self::CHECK;
        T::BITNUM_MAX >> (T::BITS - N)
    }

    /// Get a bitmask where all bits up to those set by `self.max()` are set.
    /// This may be used where the bitmask of the storage uses more bits than the actual maximum value of the integer could possibly.
    /// Usually the only way to encounter this is after arithmetic.
    ///
    /// ```
    /// # use rust_caps::checked_bits::BitInt;
    /// // Despite the BitInt storage being 64-bits, the mask() is only 7 bits because the maximum value is 127 (2^7 - 1)
    /// assert_eq!(BitInt::<u64, 64>::new_bound(20, 0..=127).mask(), 0b0111_1111);
    /// // Once the max goes over 128 an extra bit is required
    /// assert_eq!(BitInt::<u64, 64>::new_bound(20, 0..=128).mask(), 0b1111_1111);
    /// ```
    pub fn mask(&self) -> T {
        let _ = Self::CHECK;
        T::BITNUM_MAX >> (T::BITS - core::cmp::min(N, T::BITS - self.max.leading_zeros()))
    }

    /// Declare a constant value, such that the min and the max are equal to the initial value.
    ///
    /// ```
    /// # use rust_caps::checked_bits::BitInt;
    /// // Declare a 5-bit unsigned integer with constant value of 0 (thus min and max are 0)
    /// let x = BitInt::<u8, 5>::constant(0);
    /// assert_eq!(x.min(), 0);
    /// assert_eq!(x.val(), 0);
    /// assert_eq!(x.max(), 0);
    /// ```
    pub fn constant(val: T) -> Self {
        let _ = Self::CHECK;
        assert!(val & Self::toplevel_mask() == val, "Value {:x} not in mask {:x}", val, Self::toplevel_mask());

        Self {
            val,
            min: val,
            max: val,
        }
    }

    /// Declare a new value bounded between 0 and the maximum value that fits in N bits (see [BitInt::toplevel_mask])
    ///
    /// ```
    /// # use rust_caps::checked_bits::BitInt;
    /// // Declare a 5-bit unsigned integer with an initial value of 20, which is implicitly bounded between 0 and 2^5 - 1 = 31
    /// let y = BitInt::<u8, 5>::new(20);
    /// assert_eq!(y.min(), 0);
    /// assert_eq!(y.val(), 20);
    /// assert_eq!(y.max(), 31);
    /// ```
    pub fn new(val: T) -> Self {
        let _ = Self::CHECK;
        assert!(val & Self::toplevel_mask() == val, "Value 0x{:x} not in mask 0x{:x}", val, Self::toplevel_mask());

        Self {
            val,
            min: T::ZERO,
            max: Self::toplevel_mask(),
        }
    }

    /// Declare a new value bounded by an inclusive range.
    /// The range limits must also fit within N bits.
    ///
    /// ```
    /// # use rust_caps::checked_bits::BitInt;
    /// // Declare a 5-bit unsigned integer with an initial value of 20 which is bounded between 5 and 21
    /// let z = BitInt::<u8, 5>::new_bound(20, 5..=21);
    /// assert_eq!(z.min(), 5);
    /// assert_eq!(z.val(), 20);
    /// assert_eq!(z.max(), 21);
    /// ```
    pub fn new_bound(val: T, range: core::ops::RangeInclusive<T>) -> Self {
        let _ = Self::CHECK;
        let min = *range.start();
        let max = *range.end(); 

        Self::new_manual(val, min, max)
    }

    /// Declare a new value bounded by an inclusive range, declared directly instead of through [core::ops::RangeInclusive].
    /// The range limits must also fit within N bits.
    ///
    /// ```
    /// # use rust_caps::checked_bits::BitInt;
    /// // Declare a 5-bit unsigned integer with an initial value of 20 which is bounded between 5 and 21
    /// let z = BitInt::<u8, 5>::new_manual(20, 5, 21);
    /// assert_eq!(z.min(), 5);
    /// assert_eq!(z.val(), 20);
    /// assert_eq!(z.max(), 21);
    /// ```
    pub fn new_manual(val: T, min: T, max: T) -> Self {
        let _ = Self::CHECK;
        assert!(val & Self::toplevel_mask() == val, "Value {:x} not in mask {:x}", val, Self::toplevel_mask());
        assert!(min & Self::toplevel_mask() == min, "Value {:x} not in mask {:x}", min, Self::toplevel_mask());
        assert!(max & Self::toplevel_mask() == max, "Value {:x} not in mask {:x}", max, Self::toplevel_mask());
        assert!(val >= min, "Value {:x} not in bounds [{:x}, {:x}] inclusive", val, min, max);
        assert!(val <= max, "Value {:x} not in bounds [{:x}, {:x}] inclusive", val, min, max);

        Self {
            val,
            min,
            max,
        }
    }

    /// Panic if the given value doesn't fit in N-bit storage, else return it.
    ///
    /// ```
    /// # use rust_caps::checked_bits::BitInt;
    /// BitInt::<u16, 12>::assert_fits(0);
    /// BitInt::<u16, 12>::assert_fits(1);
    /// BitInt::<u16, 12>::assert_fits(2);
    /// // ...
    /// BitInt::<u16, 12>::assert_fits(0x0FFF);
    /// ```
    ///
    /// ```should_panic
    /// # use rust_caps::checked_bits::BitInt;
    /// BitInt::<u16, 12>::assert_fits(0x1FFF);
    /// ```
    /// ```should_panic
    /// # use rust_caps::checked_bits::BitInt;
    /// BitInt::<u16, 12>::assert_fits(u16::MAX);
    /// ```
    #[inline(always)]
    pub fn assert_fits(val: T) -> T {
        assert!(val & Self::toplevel_mask() == val, "Value {:x} not in mask {:x}", val, Self::toplevel_mask());
        val
    }

    /// Check if the given value fits in N-bit storage, returning Some(val) if yes and None if not.
    ///
    /// ```
    /// # use rust_caps::checked_bits::BitInt;
    /// BitInt::<u16, 12>::check(0).unwrap();
    /// BitInt::<u16, 12>::check(1).unwrap();
    /// BitInt::<u16, 12>::check(2).unwrap();
    /// // ...
    /// BitInt::<u16, 12>::check(0x0FFF).unwrap();
    ///
    /// assert!(BitInt::<u16, 12>::check(0x1FFF).is_none());
    /// assert!(BitInt::<u16, 12>::check(u16::MAX).is_none());
    /// ```
    #[inline(always)]
    pub fn check(val: T) -> Option<T> {
        if val & Self::toplevel_mask() == val {
            Some(val)
        } else {
            None
        }
    }

    /// Expand a BitInt, potentially increasing the number of bits N and the size of the number backing T.
    /// This does not change `min`, `max`, or [BitInt::mask] because the values of the number are still bounded.
    /// [BitInt::toplevel_mask] will grow to the new N.
    /// 
    /// This is necessary for some comparison operations because BitInt only supports comparisons between 
    /// values with the same backing T and bit width N.
    ///
    /// ```compile_fail
    /// # use rust_caps::checked_bits::BitInt;
    /// let x = BitInt::<u8, 5>::new_bound(20, 5..=21);
    /// let y = BitInt::<u16, 12>::constant(6);
    /// // This won't compile because x and y have different backings and N-values
    /// assert!(x.val() > y.val());
    /// ```
    ///
    /// ```
    /// # use rust_caps::checked_bits::BitInt;
    /// let x = BitInt::<u8, 5>::new_bound(20, 5..=21);
    /// // Expanding x doesn't change min, val, max, or mask
    /// let x = x.expand::<u16, 12>();
    /// assert_eq!(x.min(), 5u16);
    /// assert_eq!(x.val(), 20u16);
    /// assert_eq!(x.max(), 21u16);
    /// assert_eq!(x.mask(), 0b0001_1111); // 5 bits
    ///
    /// // This now compiles because x has been expanded to the same backing and N as y!
    /// let y = BitInt::<u16, 12>::constant(6);
    /// assert!(x.val() > y.val());
    /// ```
    pub fn expand<TB: BitNumBacking, const NB: usize>(self) -> BitInt<TB, NB> where T: Into<TB> {
        // Assert that NB >= N - otherwise the expand doesn't make sense
        let _ensure_expanded_n_greater_than_old_n = ConstCompare::<NB, N>::GREATER_THAN_EQ;
        // As long as NB >= N and TB >= T (which we know statically from T: Into<TB>) this will never fail
        BitInt::<TB, NB>::new_manual(
            self.val.into(),
            self.min.into(),
            self.max.into(),
        )
    }

    /// Shink a BitInt, potentially reducing the number of bits N and the size of the number backing T.
    /// Panics if the min, max, or value of the BitInt don't fit into the new N bits.
    /// This does not change `min`, `max`, or [BitInt::mask] because the values of the number are still bounded.
    /// [BitInt::toplevel_mask] will shrink to the new N.
    ///
    /// This can be useful once arithmetic has shrunk input values to more manageable sizes,
    /// and you want to ensure Rust code only has to think about those small values.
    /// It can be tempting to use `.try_into().unwrap()` but that does not check against the whole range of values.
    ///
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// let x = BitInt::<u16, 12>::new_bound(100, 64..=255);
    /// ```
    /// ```compile_fail
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// # let x = BitInt::<u16, 12>::new_bound(100, 64..=255);
    /// // Taking val() gives a u16, which Rust code may not want to handle
    /// let storage: u8 = x.val();
    /// ```
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// # let x = BitInt::<u16, 12>::new_bound(100, 64..=255);
    /// // It's legal to compress x down to a u8 because the numerical values fit.
    /// let x = x.shrink::<u8, 8>();
    /// // Shrinking x doesn't change min, val, max, or mask
    /// assert_eq!(x.min(), 64);
    /// assert_eq!(x.val(), 100);
    /// assert_eq!(x.max(), 255);
    /// assert_eq!(x.mask(), 0b1111_1111);
    ///
    /// // We can now store x.val() in a u8!
    /// let storage: u8 = x.val();
    /// ```
    /// ```should_panic
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// # let x = BitInt::<u16, 12>::new_bound(100, 64..=255);
    /// // You cannot shrink x to be smaller than possible storage
    /// let y: BitInt<u8, 7> = x.shrink();
    /// ```
    pub fn shrink<TB: BitNumBacking, const NB: usize>(self) -> BitInt<TB, NB> where T: TryInto<TB>, <T as TryInto<TB>>::Error: core::fmt::Debug {
        // For each of val, min, max: check that the value fits into NB bits. If not, reject outright.
        // If so, we can go through with the try_into() conversion while unwrapping because we know anything that fits into NB fits into TB.
        BitInt::<TB, NB>::new_manual(
            BitInt::<T, NB>::assert_fits(self.val).try_into().unwrap(),
            BitInt::<T, NB>::assert_fits(self.min).try_into().unwrap(),
            BitInt::<T, NB>::assert_fits(self.max).try_into().unwrap(),
        )
    }

    /// Shink a BitInt as in [BitInt::shrink] except with control flow fallibility.
    /// Returns None if the currently-considered value doesn't fit into the new N bits.
    /// Otherwise returns Some where `min` and `max` are compressed to fit into N bits.
    /// As with [CheckAdd], [CheckSub], and [CheckLeftShift] this allows control flow to reject overflow
    /// leaving the rest of the program to assume the number will not overflow.
    /// 
    ///
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// let x = BitInt::<u16, 12>::new_bound(100, 64..=255);
    /// ```
    /// ```should_panic
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// # let x = BitInt::<u16, 12>::new_bound(100, 64..=255);
    /// // Trying to compress to 7-bits without handling overflow will panic
    /// let x = x.shrink::<u8, 7>();
    /// ```
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// # let x = BitInt::<u16, 12>::new_bound(100, 64..=255);
    /// // Overflow-aware code
    /// let x = match x.shrink_overflow::<u8, 7>() {
    ///     Some(non_overflowing_value) => non_overflowing_value,
    ///     None => panic!("bail out, overflow not allowed"),
    /// };
    /// // The rest of the program can assume x is a 7-bit value
    /// assert_eq!(x.min(), 64);
    /// assert_eq!(x.val(), 100);
    /// assert_eq!(x.max(), 127); // Before, the max was 255 - now it's 127 because overflow has been rejected
    /// assert_eq!(x.mask(), 0b0111_1111); // This is a 7-bit number.
    /// ```
    pub fn shrink_overflow<TB: BitNumBacking, const NB: usize>(self) -> Option<BitInt<TB, NB>> where T: TryInto<TB>, <T as TryInto<TB>>::Error: core::fmt::Debug {
        Some(BitInt::<TB, NB>::new_manual(
            // If val doesn't fit into NB, return None
            BitInt::<T, NB>::check(self.val)?.try_into().unwrap(),
            // If we get this far we know self.val fits into NB bits. self.min must also fit in that bound.
            self.min.try_into().unwrap(),
            // self.max may not fit - if it doesn't fit, cap it at the NB-bit mask and then try_into.
            BitInt::<T, NB>::check(self.max).unwrap_or(BitInt::<T, NB>::toplevel_mask()).try_into().unwrap(),
        ))
    }

    pub fn shrink_max(self, new_max: T) -> Self {
        Self::new_manual(self.val, self.min, core::cmp::min(self.max, new_max))
    }

    /// Get the currently-considered value of this number
    pub fn val(self) -> T {
        self.val
    }
    /// Get the minimum possible value of this number
    pub fn min(self) -> T {
        self.min
    }
    /// Get the maximum possible value of this number
    pub fn max(self) -> T {
        self.max
    }
    /// Get the inclusive bound for this number
    pub fn bound(self) -> RangeInclusive<T> {
        self.min..=self.max
    }
}

/// A type equivalent to an N-bit adder.
/// Can take any [BitInt]s with widths <= N, add them together, and either panic or gracefully handle the case where overflow is possible.
pub struct CheckAdd<T: BitNumBacking, const N: usize>(PhantomData<T>);
impl<T: BitNumBacking, const N: usize> CheckAdd<T, N> {
    /// Computes `a+b`, panicking unless all possible results fit in N bits.
    ///
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// let a = BitInt::<u8, 5>::new(5);
    /// let b = BitInt::<u8, 5>::new(20);
    /// // A 6-bit adder is required here because a and b are not fixed constants - they may both be 2^5 - 1 = 31
    /// assert_eq!(CheckAdd::<u8, 6>::add(a, b).val(), 25);
    /// ```
    /// ```should_panic
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// # let a = BitInt::<u8, 5>::new(5);
    /// # let b = BitInt::<u8, 5>::new(20);
    /// // If you try to use a 5-bit adder it will result in a compile error
    /// assert_eq!(CheckAdd::<u8, 5>::add(a, b).val(), 25);
    /// ```
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// // But if a and b have known bounds, they may still fit
    /// let a = BitInt::<u8, 5>::new_bound(5, 0..=6);
    /// let b = BitInt::<u8, 5>::new_bound(20, 15..=24);
    /// // The maximum possible values in this case are 6+24 = 30 - so the result will always fit in 5 bits and a 5-bit adder can be used.
    /// let c = CheckAdd::<u8, 5>::add(a, b);
    /// assert_eq!(c.min(), 15);
    /// assert_eq!(c.val(), 25);
    /// assert_eq!(c.max(), 30);
    /// ```
    ///
    /// ```compile_fail
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// // You cannot use a smaller adder on larger ints - If you want to, you need to [BitInt::shrink] both operands to check they'll fit
    /// // Note that here all the backings are u64, but the adder declares it's 6-bits wide and can't fit the 52-bit second operand.
    /// // This will result in a compile error
    /// CheckAdd::<u64, 6>::add(BitInt::<u64, 6>::constant(1), BitInt::<u64, 52>::new(0xFF23_2325));
    /// ```
    pub fn add<TA: BitNumBacking, const NA: usize, TB: BitNumBacking, const NB: usize>(a: BitInt<TA, NA>, b: BitInt<TB, NB>) -> BitInt<T, N> where TA: Into<T>, TB: Into<T> {
        let _a_fits_inside_n_bits = ConstCompare::<N, NA>::GREATER_THAN_EQ;
        let _b_fits_inside_n_bits = ConstCompare::<N, NB>::GREATER_THAN_EQ;
        let a: BitInt<T, NA> = a.expand();
        let b: BitInt<T, NB> = b.expand();

        // This is an add. We need to make sure the result of the add stays within our toplevel_mask().
        // This can be done by checking the result of adding the max values of a and b.

        BitInt::new_manual(
            a.val.checked_add(b.val).expect("checked_add of values overflowed container"),
            a.min.checked_add(b.min).expect("checked_add of minimum values overflowed container"),
            a.max.checked_add(b.max).expect("checked_add of maximum values overflowed container")
        )
    }

    /// Computes `a+b`. If the currently-considered result would overflow N bits returns None, else returns a Some([BitInt]) where val, min, max are calcuated assuming no overflow takes place.
    /// This allows control flow to reject overflowing numbers (which in hardware would require checking the carry-out) and the program can assume no overflow occured from then on.
    ///
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// let a = BitInt::<u8, 5>::new(5);
    /// let b = BitInt::<u8, 5>::new(20);
    /// // In this case, a and b may both be 2^5-1 = 31, so the maximum value may be 62.
    /// // We can use add_overflow to detect overflow and bail out, and then subsequent code can assume no overflow happens.
    /// // NOTE that in order to test this code fully both paths need to be explored,
    /// // but once you have 100% code coverage your tests should automatically test all possible integer values if you use BitInt fully.
    /// let c = match CheckAdd::<u8, 5>::add_overflow(a, b) {
    ///     Some(non_overflowing_value) => non_overflowing_value,
    ///     None => panic!("bail out, the computation is invalid if overflow happens")
    /// };
    /// // Code proceeds as if c is a 5-bit value and no overflow occurred.
    /// assert_eq!(c.max(), 0b1_1111); // The maximum possible non-overflowing value is 31, e.g. if a = 30 and b = 1.
    /// ```
    /// ```should_panic
    /// // If add_overflow is used unnecessarily - i.e. if there is no possibility of overflow - it will panic and tell you to replace it with add() instead.
    /// // This will prevent you from adding never-taken branches to the overflow case.
    /// // The panic is data-dependent, so wherever possible do not make the bounds of a number dynamic or this function will panic inconsistently.
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// let a = BitInt::<u8, 5>::new(5);
    /// let b = BitInt::<u8, 5>::new(20);
    /// match CheckAdd::<u16, 6>::add_overflow(a, b) {
    ///     // 5-bit + 5-bit will always fit in a 6-bit. No chance of overflow, ever.
    ///     Some(non_overflowing_value) => non_overflowing_value,
    ///     None => unreachable!(), // If the None case is unreachable the code should be rewritten to use add()
    /// };
    /// ```
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// assert_eq!(CheckAdd::<u8, 5>::add_overflow(BitInt::<u8, 5>::new(5), BitInt::<u8, 5>::new(20)).unwrap().val(), 25);
    /// assert!(CheckAdd::<u8, 5>::add_overflow(BitInt::<u8, 5>::new(30), BitInt::<u8, 5>::new(30)).is_none()); // The currently-considered value overflowed
    /// ```
    ///
    /// ```compile_fail
    /// # use rust_caps::checked_bits::{BitInt, CheckAdd};
    /// // You cannot use a smaller adder on larger ints - If you want to, you need to [BitInt::shrink] both operands to check they'll fit
    /// // Note that here all the backings are u64, but the adder declares it's 6-bits wide and can't fit the 52-bit second operand.
    /// // This will result in a compile error
    /// CheckAdd::<u64, 6>::add_overflow(BitInt::<u64, 6>::constant(1), BitInt::<u64, 52>::new(0xFF23_2325));
    /// ```
    pub fn add_overflow<TA: BitNumBacking, const NA: usize, TB: BitNumBacking, const NB: usize>(a: BitInt<TA, NA>, b: BitInt<TB, NB>) -> Option<BitInt<T, N>> where TA: Into<T>, TB: Into<T> {
        let _a_fits_inside_n_bits = ConstCompare::<N, NA>::GREATER_THAN_EQ;
        let _b_fits_inside_n_bits = ConstCompare::<N, NB>::GREATER_THAN_EQ;
        let a: BitInt<T, NA> = a.expand();
        let b: BitInt<T, NB> = b.expand();

        // This can only overflow if max(a) + max(b) does not fit in N. If it fits in N then you shouldn't use this function, you should use add().
        // If we can't overflow then the None branch of this addition is never going to show up.
        let can_overflow = a.max.checked_add(b.max).and_then(BitInt::<T, N>::check).is_none();
        if !can_overflow {
            panic!("{a:?} + {b:?} cannot possibly overflow an {N}-bit number. Do not use add_safe() because that will never produce a None branch. Use add() instead.")
        }

        let val = BitInt::<T, N>::check(a.val.checked_add(b.val)?)?;
        // a.val + b.val didn't overflow, we're in a passing case.
        Some(BitInt::new_manual(
            val,
            // if this fails, all additions will overflow. This is impossible because we just checked a.val + b.val doesn't overflow => this can never fail
            a.min.checked_add(b.min).and_then(BitInt::<T, N>::check).unwrap(),
            // we only reach this point if can_overflow i.e. if a.max + b.max overflow N bits. In that case we saturate at the largest possible N-bit value.
            BitInt::<T, N>::toplevel_mask(),
        ))
    }
}

/// A type equivalent to an N-bit adder.
/// Can take any [BitInt]s with widths <= N, subtract them, and either panic or gracefully handle the case where overflow is possible.
pub struct CheckSub<T: BitNumBacking, const N: usize>(PhantomData<T>);
impl<T: BitNumBacking, const N: usize> CheckSub<T, N> {
    /// Computes `a-b`, panicking if overflow is possible.
    ///
    /// ```should_panic
    /// # use rust_caps::checked_bits::{BitInt, CheckSub};
    /// let a = BitInt::<u8, 5>::new(20);
    /// let b = BitInt::<u8, 5>::new(5);
    /// // If a and b are bounded between 0 and 2^N, then overflow is always possible
    /// // e.g. a = 0, b = 1
    /// // So this will panic even though the currently-considered values don't overflow
    /// assert_eq!(CheckSub::<u8, 5>::sub(a, b).val(), 15);
    /// ```
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckSub};
    /// // If a and b have known bounds, they may be guaranteed not to overflow
    /// let a = BitInt::<u8, 5>::new_bound(20, 15..=24);
    /// let b = BitInt::<u8, 5>::new_bound(5, 0..=6);
    /// let c = CheckSub::<u8, 5>::sub(a, b);
    /// assert_eq!(c.min(), 9); // min(a) - max(b) = 15 - 6
    /// assert_eq!(c.val(), 15);
    /// assert_eq!(c.max(), 24); // max(a) - min(b) = 24 - 0
    /// ```
    ///
    /// ```compile_fail
    /// # use rust_caps::checked_bits::{BitInt, CheckSub};
    /// // You cannot use a smaller subtractor on larger ints - If you want to, you need to [BitInt::shrink] both operands to check they'll fit
    /// // Note that here all the backings are u64, but the subtractor declares it's 6-bits wide and can't fit the 52-bit first operand.
    /// // This will result in a compile error
    /// CheckSub::<u64, 6>::sub(BitInt::<u64, 52>::new(0xFF23_2325), BitInt::<u64, 6>::constant(1));
    /// ```
    pub fn sub<TA: BitNumBacking, const NA: usize, TB: BitNumBacking, const NB: usize>(a: BitInt<TA, NA>, b: BitInt<TB, NB>) -> BitInt<T, N> where TA: Into<T>, TB: Into<T> {
        let _a_fits_inside_n_bits = ConstCompare::<N, NA>::GREATER_THAN_EQ;
        let _b_fits_inside_n_bits = ConstCompare::<N, NB>::GREATER_THAN_EQ;
        let a: BitInt<T, NA> = a.expand();
        let b: BitInt<T, NB> = b.expand();

        // checked_sub ensures it doesn't overflow T, which ensures they will stay within N as long as N >= NA and N >= NB
        // The smallest possible outcome is a.min - b.max, the largest is a.max - b.min

        BitInt::new_manual(
            a.val.checked_sub(b.val).expect("checked_sub of values overflowed container"),
            a.min.checked_sub(b.max).expect("checked_sub of min - max overflowed container"),
            a.max.checked_sub(b.min).expect("checked_sub of max - min overflowed container")
        )
    }

    /// Computes `a-b`. If `a < b` returns None, else returns a [BitInt] where val, min, max are calcuated for the `a >= b` case i.e. overflow never happens.
    /// This allows control flow to reject overflowing numbers (which in hardware would require checking the carry-out) and the program can assume no overflow occured from then on.
    ///
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckSub};
    /// let a = BitInt::<u8, 5>::new(20);
    /// let b = BitInt::<u8, 5>::new(5);
    /// // In this case, we may encounter a = 0, b = 1 and overflow.
    /// // We can use sub_overflow to detect overflow and bail out, and then subsequent code can assume no overflow happens.
    /// // NOTE that in order to test this code fully both paths need to be explored,
    /// // but once you have 100% code coverage your tests should automatically test all possible integer values if you use BitInt fully.
    /// let c = match CheckSub::<u8, 5>::sub_overflow(a, b) {
    ///     Some(non_overflowing_value) => non_overflowing_value,
    ///     None => panic!("bail out, the computation is invalid if overflow happens")
    /// };
    /// // Code proceeds as if c is a 5-bit value and no overflow occurred.
    /// assert_eq!(c.min(), 0); // The minimum possible non-overflowing value is 0.
    /// assert_eq!(c.max(), 0b1_1111); // The maximum possible non-overflowing value is 31, e.g. if a = 31 and b = 0.
    /// ```
    /// ```should_panic
    /// // If sub_overflow is used unnecessarily - i.e. if there is no possibility of overflow - it will panic and tell you to replace it with add() instead.
    /// // This will prevent you from adding never-taken branches to the overflow case.
    /// // The panic is data-dependent, so wherever possible do not make the bounds of a number dynamic or this function will panic inconsistently.
    /// # use rust_caps::checked_bits::{BitInt, CheckSub};
    /// let a = BitInt::<u8, 6>::new_bound(63, 32..=63);
    /// let b = BitInt::<u8, 5>::new_bound(5, 0..=31);
    /// match CheckSub::<u8, 6>::sub_overflow(a, b) {
    ///     // b is always smaller than a so this will never underflow.
    ///     Some(non_overflowing_value) => non_overflowing_value,
    ///     None => unreachable!()
    /// };
    /// ``` 
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckSub};
    /// assert_eq!(CheckSub::<u8, 5>::sub_overflow(BitInt::<u8, 5>::new(20), BitInt::<u8, 5>::new(5)).unwrap().val(), 15);
    /// assert!(CheckSub::<u8, 5>::sub_overflow(BitInt::<u8, 5>::new(5), BitInt::<u8, 5>::new(20)).is_none()); // The currently-considered value overflowed
    /// ```
    ///
    /// ```compile_fail
    /// # use rust_caps::checked_bits::{BitInt, CheckSub};
    /// // You cannot use a smaller subtractor on larger ints - If you want to, you need to [BitInt::shrink] both operands to check they'll fit
    /// // Note that here all the backings are u64, but the subtractor declares it's 6-bits wide and can't fit the 52-bit first operand.
    /// // This will result in a compile error
    /// CheckSub::<u64, 6>::sub_overflow(BitInt::<u64, 52>::new(0xFF23_2325), BitInt::<u64, 6>::constant(1));
    /// ```
    pub fn sub_overflow<TA: BitNumBacking, const NA: usize, TB: BitNumBacking, const NB: usize>(a: BitInt<TA, NA>, b: BitInt<TB, NB>) -> Option<BitInt<T, N>> where TA: Into<T>, TB: Into<T> {
        let _a_fits_inside_n_bits = ConstCompare::<N, NA>::GREATER_THAN_EQ;
        let _b_fits_inside_n_bits = ConstCompare::<N, NB>::GREATER_THAN_EQ;
        let a: BitInt<T, NA> = a.expand();
        let b: BitInt<T, NB> = b.expand();

        let can_overflow = a.min.checked_sub(b.max).is_none();
        if !can_overflow {
            panic!("{a:?} - {b:?} cannot possibly overflow an {N}-bit number. Do not use sub_safe() because that will never produce a None branch. Use sub() instead.")
        }

        let val = BitInt::<T, N>::check(a.val.checked_sub(b.val)?)?;
        // a.val >= b.val, we're in a passing case.
        Some(BitInt::new_manual(
            val,
            // We only reach this point of can_overflow i.e. b.max > a.min. In this function we assume a >= b in all passing cases => a.min == b.max => a.min - b.max = 0.
            T::ZERO,
            // if this fails, b.min > a.max - except we've just checked a.val >= b.val => this can never fail.
            a.max.checked_sub(b.min).unwrap(),
        ))
    }
}

/// A type equivalent to an N-bit variable shifter.
/// Can take any BitInt of width <= N, and a shift amount BitInt of width <= 8, and panic where overflow is possible.
pub struct CheckLeftShift<T: BitNumBacking, const N: usize>(PhantomData<T>);
impl<T: BitNumBacking, const N: usize> CheckLeftShift<T, N> {
    /// Computes `a << amt`, panicking if overflow is possible.
    /// 
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckLeftShift};
    /// let a = BitInt::<u8, 5>::new(31);
    /// let amt = BitInt::<u8, 2>::new(3);
    /// let s = CheckLeftShift::<u8, 8>::shl(a, amt);
    /// assert_eq!(s.min(), 0b0000_0000);
    /// assert_eq!(s.val(), 0b1111_1000);
    /// assert_eq!(s.max(), 0b1111_1000); // the maximum possible integer value of this shift has all zeroes at the bottom
    /// assert_eq!(s.mask(), 0b1111_1111); // the mask does not
    /// ```
    /// Small changes to the shift amount can have a big impact - increasing the width by one immediately pushes the data out of bounds,
    /// so it's important to pay attention to the bound on the shift by either limiting N or having a low max().
    /// ```should_panic
    /// # use rust_caps::checked_bits::{BitInt, CheckLeftShift};
    /// let a = BitInt::<u8, 5>::new(31);
    /// let amt = BitInt::<u8, 3>::new(3);
    /// // Panics because max(amt) = 7, length(max(a)) = 5, 7+5 = 12 and that doesn't fit
    /// let s = CheckLeftShift::<u8, 8>::shl(a, amt);
    /// ```
    ///
    /// You cannot use a smaller shifter on larger ints - If you want to, you need to [BitInt::shrink] the operand to check it'll fit.
    /// Note that here the backings are u64, but the shifter declares it's 6-bits wide and can't fit the 52-bit first operand.
    /// ```compile_fail
    /// # use rust_caps::checked_bits::{BitInt, CheckLeftShift};
    /// // This will result in a compile error
    /// CheckLeftShift::<u64, 6>::shl(BitInt::<u64, 52>::new(0xFF23_2325), BitInt::<u8, 1>::constant(1));
    /// ```
    pub fn shl<TA: BitNumBacking, const NA: usize, const NB: usize>(a: BitInt<TA, NA>, amt: BitInt<u8, NB>) -> BitInt<T, N> where TA: Into<T> {
        let _a_fits_inside_n_bits = ConstCompare::<N, NA>::GREATER_THAN_EQ;
        let a: BitInt<T, NA> = a.expand();

        // This is a shift left. We need to make sure no bits will ever be shifted out of the top.

        // Track the top possible set bit of the value - use a.mask() for this instead of TOPLEVEL_MASK because that takes into account the actual maximum value of the integer
        // the total length of the resulting number is at most (mask_length) + amt.max, and the maximum allowed total length is N.
        let mask_length = T::BITS - a.mask().leading_zeros();
        if (amt.max as usize) + mask_length > N {
            panic!("left shift of (a value up to {} bits wide) by (up to {}) into (a container of {} bits) does not fit", mask_length, amt.max, N);
        }

        BitInt::new_manual(
            a.val << amt.val.into(),
            a.min << amt.min.into(),
            a.max << amt.max.into()
        )
    }

    /// Computes `a << amt`.
    /// If the minimum possible value of `amt` can result in an overflow, panics.
    /// If the currently-considered values of `a` and `amt` result in an overflow, panics.
    /// Otherwise returns a [BitInt] where val, min, max are calcuated for the no-overflow case.
    /// This allows program to assume no overflow happens. Because the overflow case is not easily detected in hardware, *THIS PROPERTY MUST BE MANUALLY PROVEN SOUND*.
    /// 
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckLeftShift};
    /// let x = BitInt::<u8, 8>::new(5);
    /// // Sometimes a data-dependent condition can prove a shift is safe in ways the checker can't comprehend.
    /// let dynamic_data = 15;
    /// if x.val() > dynamic_data {
    ///     panic!("x cannot be greater than {dynamic_data}!");
    /// }
    /// let shift_computed_from_dynamic_data = BitInt::<u8, 8>::new(4);
    /// // For dynamic_data=15 x is 0..=15 (at most 4 bits) and the shift computed based on dynamic data is 4.
    /// // This is safe to shift into an 8-bit number.
    /// // However, the checker only sees x.max() = 255, and for whatever reason the condition is too complicated to actually change x.max().
    /// ```
    /// ```should_panic
    /// # use rust_caps::checked_bits::{BitInt, CheckLeftShift};
    /// # let x = BitInt::<u8, 8>::new(5);
    /// # let shift_computed_from_dynamic_data = BitInt::<u8, 8>::new(4);
    /// /// In this case shl will panic
    /// CheckLeftShift::<u8, 8>::shl(x, shift_computed_from_dynamic_data);
    /// ```
    /// ```
    /// # use rust_caps::checked_bits::{BitInt, CheckLeftShift};
    /// # let x = BitInt::<u8, 8>::new(5);
    /// # let shift_computed_from_dynamic_data = BitInt::<u8, 8>::new(4);
    /// /// but shl_proven_no_overflow will be fine
    /// let x = CheckLeftShift::<u8, 8>::shl_proven_no_overflow(x, shift_computed_from_dynamic_data);
    /// // Currently we don't try to reason about min/max correctly, so the bounds explode
    /// assert_eq!(x.min(), 0x0);
    /// assert_eq!(x.val(), 0x50); // 5 << 4
    /// assert_eq!(x.max(), 0xFF);
    /// ```
    ///
    /// You cannot use a smaller shifter on larger ints - If you want to, you need to [BitInt::shrink] the operand to check it'll fit.
    /// Note that here the backings are u64, but the shifter declares it's 6-bits wide and can't fit the 52-bit first operand.
    /// ```compile_fail
    /// # use rust_caps::checked_bits::{BitInt, CheckLeftShift};
    /// // This will result in a compile error
    /// CheckLeftShift::<u64, 6>::shl_proven_no_overflow(BitInt::<u64, 52>::new(0xFF23_2325), BitInt::<u8, 1>::constant(1));
    /// ```
    pub fn shl_proven_no_overflow<TA: BitNumBacking, const NA: usize, const NB: usize>(a: BitInt<TA, NA>, amt: BitInt<u8, NB>) -> BitInt<T, N> where TA: Into<T> {
        let _a_fits_inside_n_bits = ConstCompare::<N, NA>::GREATER_THAN_EQ;
        let a: BitInt<T, NA> = a.expand();

        let val_length = T::BITS - a.val.leading_zeros();
        if (amt.val as usize) + val_length > N {
            panic!("left shift of (a value 0x{:x} which is {} bits wide) by {} into (a container of {} bits) cannot possibly fit - your no-overflow proof is faulty :)", a.val, val_length, amt.val, N);
        }

        // It may be possible to reason about the bounds but I'm not 100% confident. At this point just be happy the actual value didn't overflow.
        // In the specific case of a.val == 0, amt.val is allowed to be *anything*, even something that overflows T! but Rust << will pick that up in debug mode.
        // so if a.val == 0 just return 0
        BitInt::new(
            if a.val > T::ZERO { a.val << amt.val.into() } else { T::ZERO },
        )
    }
}

/// Grows a value from N bits to NA bits by shifting it left (padding the right with zeros).
///
/// ```
/// # use rust_caps::checked_bits::{BitInt, right_pad};
/// let x = BitInt::<u16, 12>::new(1 << 11);
/// assert_eq!(x.min(), 0);
/// assert_eq!(x.val(), 0x800);
/// assert_eq!(x.max(), 0xFFF);
/// assert_eq!(x.mask(), 0xFFF);
/// // shifts left by (16-12) = 4
/// let r: BitInt<u16, 16> = right_pad(x);
/// assert_eq!(r.min(), 0);
/// assert_eq!(r.val(), 0x800_0);
/// assert_eq!(r.max(), 0xFFF_0); // max has zeroes at the bottom
/// assert_eq!(r.mask(), 0xFFF_F); // mask does not
/// ```
///
/// Cannot be used to shrink something
/// ```compile_fail
/// # use rust_caps::checked_bits::{BitInt, right_pad};
/// let x = BitInt::<u16, 12>::new(1 << 11);
/// let r: BitInt<u16, 1> = right_pad(x);
/// ```
pub fn right_pad<T: BitNumBacking, const N: usize, TA: BitNumBacking, const NA: usize>(a: BitInt<TA, NA>) -> BitInt<T, N> where TA: Into<T> {
    let _growing_not_shrinking = ConstCompare::<N, NA>::GREATER_THAN;

    let a: BitInt<T, NA> = a.expand();

    let nshift: usize = N - NA;

    BitInt::new_manual(
        a.val << nshift,
        a.min << nshift,
        a.max << nshift,
    )
}

/// Shrinks a value from N bits to NA bits by shifting it right.
///
/// ```
/// # use rust_caps::checked_bits::{BitInt, const_shift_right};
/// let x = BitInt::<u16, 16>::new_bound(1 << 15, 0xFF..=0xFFFF);
/// assert_eq!(x.min(), 0xFF);
/// assert_eq!(x.val(), 0x800_0);
/// assert_eq!(x.max(), 0xFFFF);
/// assert_eq!(x.mask(), 0xFFFF);
/// // shifts right by (16-12) = 4
/// let r: BitInt<u16, 12> = const_shift_right(x);
/// assert_eq!(r.min(), 0xF); // truncated
/// assert_eq!(r.val(), 0x800);
/// assert_eq!(r.max(), 0xFFF); // max and mask are also truncated
/// assert_eq!(r.mask(), 0xFFF);
/// ```
///
/// Cannot be used to grow something
/// ```compile_fail
/// # use rust_caps::checked_bits::{BitInt, const_shift_right};
/// let x = BitInt::<u16, 12>::new(1 << 11);
/// let r: BitInt<u16, 16> = const_shift_right(x);
/// ```
pub fn const_shift_right<T: BitNumBacking, const N: usize, TA: BitNumBacking, const NA: usize>(a: BitInt<TA, NA>) -> BitInt<T, N> where TA: TryInto<T>, <TA as TryInto<T>>::Error: core::fmt::Debug {
    let _shrinking_not_growing = ConstCompare::<N, NA>::LESS_THAN;

    let nshift: usize = NA - N;

    // shift-right is a truncation, min(trunc(val)) = trunc(min(val)) and same for max.
    // thought experiment in decimal terms: a number between 11 and 107 will be between 1 and 10 after truncating by one place.

    // We know val, min, max fit into NA bits.
    // We know that shifting them down will make them fit into N bits by definition of nshift.
    // We know that N bits fit into T, becuase the BitInt::new_manual call doesn't make a compile error.
    // => we know val, min, max fit into T when shifted down => call .try_into().unwrap()

    BitInt::new_manual(
        (a.val >> nshift).try_into().unwrap(),
        (a.min >> nshift).try_into().unwrap(),
        (a.max >> nshift).try_into().unwrap(),
    )
}

pub fn concat<T: BitNumBacking, const N: usize, TA: BitNumBacking, const NA: usize, TB: BitNumBacking, const NB: usize>(a: BitInt<TA, NA>, b: BitInt<TB, NB>) -> BitInt<T, N> where TA: Into<T>, TB: Into<T> {
    assert_eq!(N, NA+NB);
    let a: BitInt<T, NA> = a.expand();
    let b: BitInt<T, NB> = b.expand();

    BitInt::new_manual(
        (a.val << NB) | b.val,
        (a.min << NB) | b.min,
        (a.max << NB) | b.max
    )
}
