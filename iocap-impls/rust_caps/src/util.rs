use core::ops::{BitOr, Shl, Shr};

/// Converts a slice of 16 bytes to a u128.
/// Returns None if the slice is not 16 bytes long.
pub fn byte_slice_to_u128(bytes: &[u8]) -> Option<u128> {
    match bytes.len() {
        16 => Some(u128::from_le_bytes(bytes.try_into().unwrap())),
        _ => None,
    }
}

pub fn byte_array_to_u128(bytes: &[u8; 16]) -> u128 {
    u128::from_le_bytes(*bytes)
}

pub fn u128_to_byte_array(data: u128) -> [u8; 16] {
    data.to_le_bytes()
}

pub const fn le_literal(l: u128) -> u128 {
    u128::from_le_bytes(l.to_be_bytes())
}

pub trait NumAlign {
    fn align_down(self, position: usize) -> Self;
}

pub fn align_to<T: Shl<usize, Output=T> + Shr<usize, Output=T>>(num: T, position: usize) -> T {
    (num >> position) << position
}

/// Generate a number aligned to *exactly* `position`, i.e. the first bit set is bit `#position`
pub fn align_to_exact<T: Shl<usize, Output=T> + Shr<usize, Output=T> + BitOr<Output=T> + From<bool>>(num: T, position: usize) -> T {
    let one: T = true.into();
    align_to(num, position) | (one << position)
}

#[cfg(test)]
mod test {
    use crate::util::{align_to, align_to_exact};

    #[test]
    fn test_align_to() {
        assert_eq!(align_to(0b1101011101011101110111011u32, 3), 0b1101011101011101110111000);
        assert_eq!(align_to(0b1101011101011101110111011u32, 4), 0b1101011101011101110110000);
        assert_eq!(align_to(0b1101011101011101110111011u32, 7), 0b1101011101011101110000000);
        assert_eq!(align_to(0b1101011101011101110111011u32, 9), 0b1101011101011101000000000);
    }

    #[test]
    fn test_align_to_exact() {
        assert_eq!(align_to_exact(0u64, 12).trailing_zeros(), 12);
        assert_eq!(align_to_exact(1u64, 12).trailing_zeros(), 12);
        assert_eq!(align_to_exact(0b1101011101011101110111011u64, 12).trailing_zeros(), 12);
    }
}