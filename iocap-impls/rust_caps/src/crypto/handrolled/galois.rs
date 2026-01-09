//! Galois Field Multiplication for:
//! - bytes in the field GF(2^8) (addition and multiplication MOD AES constant m) for AES
//! - words, made of four separate bytes in the field GF(2^8) (addition only) for AES
//! - 128-bit values in the field GF(2^128) for AES-GCM (addition and multiplication MOD AES constant R)

use core::ops::{Add, AddAssign, Index, IndexMut, Mul, MulAssign};

/// From the AES spec
///
/// "The transformation of bytes in which the polynomial representation of the input byte is multiplied by x, modulo m(x), to produce the polynomial representation of the output byte"
fn x_times(b: u8) -> u8 {
    // The polynomial m(x) is 1.x0 + 1.x1 + 0.x2 + 1.x3, 1.x4, 0.x5, 0.x6, 0.x7, 1.x8.
    // The missing x^8 is handled by the match case below, see the definition of x_times(b) in eq (4.5)/section 4.2
    const M_OF_X_MINUS_X8: u8 = 0b0001_1011;
    match b >> 7 {
        0 => b << 1,
        _ => (b << 1) ^ M_OF_X_MINUS_X8,
    }
}

/// Sec 4: addition and multiplication in the Galois Field GF(2^8)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GaloisFieldByte(pub u8);
impl GaloisFieldByte {
    pub fn times_x(&self) -> Self {
        GaloisFieldByte(x_times(self.0))
    }
}
impl Add for GaloisFieldByte {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}
impl AddAssign for GaloisFieldByte {
    fn add_assign(&mut self, rhs: Self) {
        // Sec 4.1: In GF(2^8), "two bytes can be added by applying the exclusive OR operation to each pair of corresponding bits in the bytes"
        self.0 = self.0 ^ rhs.0
    }
}
impl Mul for GaloisFieldByte {
    type Output = Self;

    fn mul(mut self, rhs: Self) -> Self::Output {
        self *= rhs;
        self
    }
}
impl MulAssign for GaloisFieldByte {
    fn mul_assign(&mut self, rhs: Self) {
        // Polynomial multiplication in GF(2^8), see sec 4.2
        // TODO implement something more efficient?
        let b = *self;
        let c = rhs.0;

        let mut accum = GaloisFieldByte(0u8);
        // represents b*(x^i)
        let mut b_times_x_to_the_i = b;

        // for each bit in c
        for i in 0..8 {
            // If the i-th bit of c is set
            if (c >> i) & 1 == 1 {
                // "Add" b*(x^i) to the accumulator
                accum = accum + b_times_x_to_the_i;
            }
            // i => i+1
            // therefore b*x^i => b*x^i * x
            b_times_x_to_the_i = b_times_x_to_the_i.times_x();
        }

        *self = accum;
    }
}
impl From<u8> for GaloisFieldByte {
    fn from(value: u8) -> Self {
        Self(value)
    }
}
impl From<GaloisFieldByte> for u8 {
    fn from(value: GaloisFieldByte) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GaloisFieldWord(pub [GaloisFieldByte; 4]);
impl GaloisFieldWord {
    pub const fn from_bytes(bs: [u8; 4]) -> Self {
        GaloisFieldWord([
            GaloisFieldByte(bs[0]),
            GaloisFieldByte(bs[1]),
            GaloisFieldByte(bs[2]),
            GaloisFieldByte(bs[3]),
        ])
    }
    pub fn from_byte_slice(bs: &[u8]) -> Self {
        GaloisFieldWord([
            GaloisFieldByte(bs[0]),
            GaloisFieldByte(bs[1]),
            GaloisFieldByte(bs[2]),
            GaloisFieldByte(bs[3]),
        ])
    }
}
impl Add for GaloisFieldWord {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}
impl AddAssign for GaloisFieldWord {
    fn add_assign(&mut self, rhs: Self) {
        self.0[0] += rhs.0[0];
        self.0[1] += rhs.0[1];
        self.0[2] += rhs.0[2];
        self.0[3] += rhs.0[3];
    }
}
impl Index<usize> for GaloisFieldWord {
    type Output = GaloisFieldByte;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}
impl IndexMut<usize> for GaloisFieldWord {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}
impl Default for GaloisFieldWord {
    fn default() -> Self {
        Self([GaloisFieldByte(0); 4])
    }
}

/// GF(2^128) polynomial with addition, multiplication, division? support.
///
/// See https://csrc.nist.rip/groups/ST/toolkit/BCM/documents/proposedmodes/gcm/gcm-spec.pdf
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GaloisFieldU128(pub u128);
impl GaloisFieldU128 {
    pub fn shifted_left(self) -> Self {
        Self(self.0 << 1)
    }
}
impl Add for GaloisFieldU128 {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}
impl AddAssign for GaloisFieldU128 {
    fn add_assign(&mut self, rhs: Self) {
        self.0 = self.0 ^ rhs.0;
    }
}
impl Mul for GaloisFieldU128 {
    type Output = Self;

    fn mul(mut self, rhs: Self) -> Self::Output {
        self *= rhs;
        self
    }
}
impl MulAssign for GaloisFieldU128 {
    /// From GCM spec section 2.5
    fn mul_assign(&mut self, rhs: Self) {
        // For GCM R = 1x^0 + 1x^1 + 1x^2 + 0x^3 + 0x^4 + 0x^5 + 0x^6 + 1x^7 + ... + 1x^128
        const R_MINUS_X128: GaloisFieldU128 = GaloisFieldU128(0b10000111);

        let x = *self;
        let y = rhs;
        let mut z = GaloisFieldU128(0);
        let mut v = x;

        for i in 0..=127 {
            if (y.0 >> i) & 1 == 1 {
                z = z + v;
            }
            // The GCM spec uses the terminology "rightshift" for this,
            // but their bit vectors are the other way around!!!
            if (v.0 >> 127) == 0 {
                v = v.shifted_left();
            } else {
                v = v.shifted_left() + R_MINUS_X128;
            }
        }

        self.0 = z.0;
    }
}
impl From<u128> for GaloisFieldU128 {
    fn from(value: u128) -> Self {
        Self(value)
    }
}
impl From<GaloisFieldU128> for u128 {
    fn from(value: GaloisFieldU128) -> Self {
        value.0
    }
}

#[cfg(test)]
mod test {
    use assert_hex::assert_eq_hex;

    use crate::crypto::handrolled::galois::{x_times, GaloisFieldByte, GaloisFieldU128};

    #[test]
    fn test_x_times_0x57() {
        // Sec 4.2 lists example solutions for x_times()
        let val = 0x57;
        let val_x1 = x_times(val);
        assert_eq_hex!(val_x1, 0xae);
        let val_x2 = x_times(val_x1);
        assert_eq_hex!(val_x2, 0x47);
        let val_x3 = x_times(val_x2);
        assert_eq_hex!(val_x3, 0x8e);
        let val_x4 = x_times(val_x3);
        assert_eq_hex!(val_x4, 0x07);
        let val_x5 = x_times(val_x4);
        assert_eq_hex!(val_x5, 0x0e);
        let val_x6 = x_times(val_x5);
        assert_eq_hex!(val_x6, 0x1c);
        let val_x7 = x_times(val_x6);
        assert_eq_hex!(val_x7, 0x38);
    }

    #[test]
    fn test_galois_field_byte_mult() {
        // Sec 4.2 lists an example for galois field multiplication
        let res = GaloisFieldByte(0x57) * GaloisFieldByte(0x13);
        assert_eq_hex!(res, GaloisFieldByte(0xfe));

        let mut val = GaloisFieldByte(0x57);
        val *= GaloisFieldByte(0x13);
        assert_eq_hex!(val, GaloisFieldByte(0xfe));
    }

    #[test]
    fn test_galois_field_u128_mult() {
        // From https://www.intel.com/content/dam/develop/external/us/en/documents/clmul-wp-rev-2-02-2014-04-20.pdf
        let a = GaloisFieldU128(0x7b5b54657374566563746f725d53475d);
        let b = GaloisFieldU128(0x48692853686179295b477565726f6e5d);
        let expected_res = GaloisFieldU128(0x40229a09a5ed12e7e4e10da323506d2);

        assert_eq_hex!(a * b, expected_res);
    }
}
