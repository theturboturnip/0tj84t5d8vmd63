use assert_hex::assert_eq_hex;

use crate::{capability::{v2024::perms::CapPermsChain, v2024::Cap2024, AddrRange, CapDecodeErr, CapEncDecErr, CapEncodeErr, CapPerms, Capability}, crypto::Aes128HandrolledSig, util::{align_to, align_to_exact}};

use super::{Cap2024_11, EncodedInitialRange};

fn check_pack_valid(r: AddrRange) -> Result<AddrRange, CapEncodeErr> {
    // pack_base_and_length returns CapabilityErr if the decoded range doesn't encompass the expected range
    // it may be larger!
    let (_, new_r) = EncodedInitialRange::pack_base_and_length(&r)?;
    Ok(new_r)
}
fn check_pack_valid_but_not_exact(r: AddrRange) -> Result<AddrRange, CapEncodeErr> {
    let (_, new_r) = EncodedInitialRange::pack_base_and_length(&r)?;
    if new_r == r {
        panic!("Test failed: initial range {:x?} encoded/decoded to {:x?} which is exactly the same", r, new_r);
    }
    if !new_r.encompasses(&r) {
        panic!("Algorithm failure: computed range {:x?} doesn't encompass requested range {:x?}", new_r, r)
    }
    Ok(new_r)
}
fn check_pack_exact(r: AddrRange) -> Result<(), CapEncodeErr> {
    EncodedInitialRange::pack_base_and_length_exact(&r)?;
    Ok(())
}

// 2024_02 allows a 54-bit base address, 2024_11 requires 54-bit
const BASE_ADDR: u64 = 0x3a8fe3_6ac76c00;
fn base_aligned_to_exactly(pos: usize) -> u64 {
    align_to_exact(BASE_ADDR, pos)
}

// TODO test implicit-1=false areas in weirder cases - rounding?

#[test]
fn exact_base_exact_length_exhaustive() -> Result<(), CapEncodeErr> {
    let test_base_length = |base_align: usize, length: u64, elem_width_log_2: u8, elem_count_bits: u8, implicit_one: bool| -> Result<(), CapEncodeErr> {
        let base = base_aligned_to_exactly(base_align);
        // If base+length creates overflow, calculate a new base with the requested alignment supporting the requested length
        let base = match base.checked_add(length) {
            Some(_) => base,
            None => {
                // overflow happened
                let min_base = u64::MAX - length;
                align_to_exact(min_base, base_align)
            }
        };
        let r = EncodedInitialRange::pack_base_and_length_exact(&AddrRange::base_len_64(base, length)).unwrap();
        let partial_decode = r.partial_decode();
        assert_eq!(elem_count_bits + implicit_one as u8, partial_decode.log_max_count.val());
        assert_eq!(elem_width_log_2, partial_decode.elem_width_log2_minus_10.val() + 10);
        Ok(())
    };

    // implicit-one=false quadrant 0
    test_base_length(10, 1 << 10, 10, 5, false)?;
    test_base_length(10, (1 << 10) + (1 << 10), 10, 5, false)?;
    test_base_length(10, 1 << 11, 10, 5, false)?;
    test_base_length(10, 1 << 13, 10, 5, false)?;
    test_base_length(10, 1 << 14, 10, 5, false)?;
    test_base_length(10, 1 << 15, 10, 5, false)?;
    // the maximum implicit-one=false value storable is 1 << 15
    test_base_length(10, (1 << 15) + (1 << 10), 10, 5, true)?;

    // implicit-one=true quadrant 0
    // Quadrant 0, 10-bit precision base, lengths up to 2^22.
    for i in 15..=21 {
        const Q0_MIN_LENGTH_MANTISSA: u64 = 0b100001;
        const Q0_MAX_LENGTH_MANTISSA: u64 = 0b111111;

        // for anything in the range (2^i exclusive) to (2^(i+1) inclusive), it should be stored with 5(+implicit1) count bits, an elem_width_log_2 = (i - 1)
        // test the minimum nonzero mantissa (i.e. 2^i + 2^(i - elem_count_bits)), and the maximum nonzero mantissa (i.e. all 1s between 2^i and 2^(i - elem_count_bits))
        let elem_count_bits = 5;
        let elem_width_log_2 = i - elem_count_bits;
        let implicit_one = true;
        let base_align = 10;
        test_base_length(base_align, Q0_MIN_LENGTH_MANTISSA << elem_width_log_2, elem_width_log_2, elem_count_bits, implicit_one)?;
        test_base_length(base_align, Q0_MAX_LENGTH_MANTISSA << elem_width_log_2, elem_width_log_2, elem_count_bits, implicit_one)?;
        test_base_length(base_align, 1 << (i + 1), elem_width_log_2, elem_count_bits, implicit_one)?;
    }

    // Explicit manual tests
    test_base_length(10, 1 << 16, 10, 5, true)?;
    test_base_length(10, 1 << 17, 11, 5, true)?;
    test_base_length(10, 1 << 18, 12, 5, true)?;
    test_base_length(10, 1 << 19, 13, 5, true)?;
    test_base_length(10, 1 << 20, 14, 5, true)?;
    test_base_length(10, 1 << 21, 15, 5, true)?;
    test_base_length(10, 1 << 22, 16, 5, true)?;

    // implicit-one=false quadrant 1
    test_base_length(17, (1 << 22) + (1 << 17), 17, 12, false)?;
    test_base_length(17, 1 << 23, 17, 12, false)?;
    test_base_length(17, 1 << 24, 17, 12, false)?;
    test_base_length(17, 1 << 25, 17, 12, false)?;
    test_base_length(17, 1 << 26, 17, 12, false)?;
    test_base_length(17, 1 << 27, 17, 12, false)?;
    test_base_length(17, 1 << 28, 17, 12, false)?;
    test_base_length(17, 1 << 29, 17, 12, false)?;
    // set all possible bits in the count (12)
    test_base_length(17, 0xFFF << 17, 17, 12, false)?;
    // the maximum implicit-one=false value storable is 1 << 29
    test_base_length(17, (1 << 29) + (1 << 17), 17, 12, true)?;

    // implicit-one=true quadrant 1
    // 13-bit mantissa
    for i in 29..=35 {
        const Q1_MIN_LENGTH_MANTISSA: u64 = (1 << 12) | 1;
        const Q1_MAX_LENGTH_MANTISSA: u64 = 0x1FFF; // all bits in the mantissa set (12 + implicit one)

        // for anything in the range (2^i exclusive) to (2^(i+1) inclusive), it should be stored with 12(+implicit1) count bits, an elem_width_log_2 = (i - 1)
        // test the minimum nonzero mantissa (i.e. 2^i + 2^(i - elem_count_bits)), and the maximum nonzero mantissa (i.e. all 1s between 2^i and 2^(i - elem_count_bits))
        let elem_count_bits = 12;
        let elem_width_log_2 = i - elem_count_bits;
        let implicit_one = true;
        let base_align = 17;
        test_base_length(base_align, Q1_MIN_LENGTH_MANTISSA << elem_width_log_2, elem_width_log_2, elem_count_bits, implicit_one)?;
        test_base_length(base_align, Q1_MAX_LENGTH_MANTISSA << elem_width_log_2, elem_width_log_2, elem_count_bits, implicit_one)?;
        test_base_length(base_align, 1 << (i + 1), elem_width_log_2, elem_count_bits, implicit_one)?;
    }

    // Explicit manual tests
    test_base_length(17, 1 << 30, 17, 12, true)?;
    test_base_length(17, 1 << 31, 18, 12, true)?;
    test_base_length(17, 1 << 32, 19, 12, true)?;
    test_base_length(17, 1 << 33, 20, 12, true)?;
    test_base_length(17, 1 << 34, 21, 12, true)?;
    test_base_length(17, 1 << 35, 22, 12, true)?;
    test_base_length(17, 1 << 36, 23, 12, true)?;

    // implicit-one=false quadrant 2
    test_base_length(24, (1 << 36) + (1 << 24), 24, 19, false)?;
    test_base_length(24, 1 << 37, 24, 19, false)?;
    test_base_length(24, 1 << 38, 24, 19, false)?;
    test_base_length(24, 1 << 39, 24, 19, false)?;
    test_base_length(24, 1 << 40, 24, 19, false)?;
    test_base_length(24, 1 << 41, 24, 19, false)?;
    test_base_length(24, 1 << 42, 24, 19, false)?;
    test_base_length(24, 1 << 43, 24, 19, false)?;
    // set all possible bits in the count (19)
    test_base_length(24, 0x7FFFF << 24, 24, 19, false)?;
    // the maximum implicit-one=false value storable is 1 << 43
    test_base_length(24, (1 << 43) + (1 << 24), 24, 19, true)?;

    // implicit-one=true quadrant 2
    // 13-bit mantissa
    for i in 43..=49 {
        const Q2_MIN_LENGTH_MANTISSA: u64 = (1 << 19) | 1;
        const Q2_MAX_LENGTH_MANTISSA: u64 = 0xFFFFF; // all bits in the mantissa set (19 + implicit one)

        // for anything in the range (2^i exclusive) to (2^(i+1) inclusive), it should be stored with 19(+implicit1) count bits, an elem_width_log_2 = (i - 1)
        // test the minimum nonzero mantissa (i.e. 2^i + 2^(i - elem_count_bits)), and the maximum nonzero mantissa (i.e. all 1s between 2^i and 2^(i - elem_count_bits))
        let elem_count_bits = 19;
        let elem_width_log_2 = i - elem_count_bits;
        let implicit_one = true;
        let base_align = 24;
        test_base_length(base_align, Q2_MIN_LENGTH_MANTISSA << elem_width_log_2, elem_width_log_2, elem_count_bits, implicit_one)?;
        test_base_length(base_align, Q2_MAX_LENGTH_MANTISSA << elem_width_log_2, elem_width_log_2, elem_count_bits, implicit_one)?;
        test_base_length(base_align, 1 << (i + 1), elem_width_log_2, elem_count_bits, implicit_one)?;
    }

    // Explicit manual tests
    test_base_length(24, 1 << 44, 24, 19, true)?;
    test_base_length(24, 1 << 45, 25, 19, true)?;
    test_base_length(24, 1 << 46, 26, 19, true)?;
    test_base_length(24, 1 << 47, 27, 19, true)?;
    test_base_length(24, 1 << 48, 28, 19, true)?;
    test_base_length(24, 1 << 49, 29, 19, true)?;
    test_base_length(24, 1 << 50, 30, 19, true)?;

    // implicit-one=false quadrant 3
    test_base_length(31, (1 << 50) + (1 << 31), 31, 26, false)?;
    test_base_length(31, 1 << 51, 31, 26, false)?;
    test_base_length(31, 1 << 52, 31, 26, false)?;
    test_base_length(31, 1 << 53, 31, 26, false)?;
    test_base_length(31, 1 << 54, 31, 26, false)?;
    test_base_length(31, 1 << 55, 31, 26, false)?;
    test_base_length(31, 1 << 56, 31, 26, false)?;
    test_base_length(31, 1 << 57, 31, 26, false)?;
    // set all possible bits in the count (26)
    test_base_length(31, 0x3FFFFFF << 31, 31, 26, false)?;
    // the maximum implicit-one=false value storable is 1 << 57
    test_base_length(31, (1 << 57) + (1 << 31), 31, 26, true)?;

    // implicit-one=true quadrant 3
    // 13-bit mantissa
    for i in 57..=63 {
        const Q3_MIN_LENGTH_MANTISSA: u64 = (1 << 26) | 1;
        const Q3_MAX_LENGTH_MANTISSA: u64 = 0x7FFFFFF; // all bits in the mantissa set (26 + implicit one)

        // for anything in the range (2^i exclusive) to (2^(i+1) inclusive), it should be stored with 26(+implicit1) count bits, an elem_width_log_2 = (i - 1)
        // test the minimum nonzero mantissa (i.e. 2^i + 2^(i - elem_count_bits)), and the maximum nonzero mantissa (i.e. all 1s between 2^i and 2^(i - elem_count_bits))
        let elem_count_bits = 26;
        let elem_width_log_2 = i - elem_count_bits;
        let implicit_one = true;
        let base_align = 31;
        test_base_length(base_align, Q3_MIN_LENGTH_MANTISSA << elem_width_log_2, elem_width_log_2, elem_count_bits, implicit_one)?;
        test_base_length(base_align, Q3_MAX_LENGTH_MANTISSA << elem_width_log_2, elem_width_log_2, elem_count_bits, implicit_one)?;
        if i != 63 {
            // If i == 63, don't test this - that's the full 64-bit addrspace, which is a special case for later
            test_base_length(base_align, 1 << (i + 1), elem_width_log_2, elem_count_bits, implicit_one)?;
        }
    }

    // Explicit manual tests
    test_base_length(31, 1 << 58, 31, 26, true)?;
    test_base_length(31, 1 << 59, 32, 26, true)?;
    test_base_length(31, 1 << 60, 33, 26, true)?;
    test_base_length(31, 1 << 61, 34, 26, true)?;
    test_base_length(31, 1 << 62, 35, 26, true)?;
    test_base_length(31, 1 << 63, 36, 26, true)?;

    // Special case: the full address space
    let r = EncodedInitialRange::pack_base_and_length_exact(&AddrRange::FULL_ADDR_SPACE)?;
    let partial_decode = r.partial_decode();
    assert_eq!(27, partial_decode.log_max_count.val()); // 26 elem_count_bits plus implicit-1
    assert_eq!(37, partial_decode.elem_width_log2_minus_10.val() + 10);

    Ok(())
}

#[test]
fn exact_base_exact_length() -> Result<(), CapEncodeErr> {
    // Quadrant 0, 10-bit precision base, lengths up to 2^22, 5-bits of length-precision (i.e. if length is >2^21 the bottom 16 bits are zero)
    let base: u64 = base_aligned_to_exactly(10);
    let length: u64 = 0b11111_00000_00000;
    check_pack_exact(AddrRange::base_len_64(base, length))?;
    // Quadrant 1, 17-bit precision base, lengths up to 2^36, 12-bits of length-precision (i.e. if length is >2^35 the bottom 23 bits are zero)
    let base: u64 = base_aligned_to_exactly(17);
    let length: u64 = 0xFFF_00000;
    check_pack_exact(AddrRange::base_len_64(base, length))?;
    // Quadrant 2, 24-bit precision base, lengths up to 2^50, 19-bits of length-precision (i.e. if length is >2^49 the bottom 30 bits are zero)
    let base: u64 = base_aligned_to_exactly(24);
    let length: u64 = 0x7FFFF_0000000;
    check_pack_exact(AddrRange::base_len_64(base, length))?;
    // Quadrant 3, 31-bit precision base, lengths up to 2^64, 26-bits of length-precision (i.e. if length is >2^63 the bottom 37 bits are zero)
    let base: u64 = base_aligned_to_exactly(31);
    let length: u64 = 0x3FFFFFF_000000000;
    check_pack_exact(AddrRange::base_len_64(base, length))?;
    check_pack_exact(AddrRange::FULL_ADDR_SPACE)?;
    Ok(())
}

#[test]
fn inexact_base_exact_length() -> Result<(), CapEncodeErr> {
    // In this case, run the same tests as exact_base_exact_length except with one subtracted from the base.
    // That means the start needs to be offset by one base-block, and the length needs to be bumped up by one base-block to compensate

    // Quadrant 0, 10-bit precision base, lengths up to 2^22, 5-bits of length-precision (i.e. if length is >2^21 the bottom 16 bits are zero)
    let base: u64 = base_aligned_to_exactly(10) - 1;
    let length: u64 = 0b11111_00000_00000;
    check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;

    let base: u64 = base_aligned_to_exactly(10) + 1;
    let length: u64 = 0b11111_00000_00000;
    check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;

    // Quadrant 1, 17-bit precision base, lengths up to 2^36, 12-bits of length-precision (i.e. if length is >2^35 the bottom 23 bits are zero)
    let base: u64 = base_aligned_to_exactly(17) - 1;
    let length: u64 = 0xFFF_00000;
    check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;

    // Quadrant 2, 24-bit precision base, lengths up to 2^50, 19-bits of length-precision (i.e. if length is >2^49 the bottom 30 bits are zero)
    let base: u64 = base_aligned_to_exactly(24) - 1;
    let length: u64 = 0x7FFFF_0000000;
    check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;

    // Quadrant 3, 31-bit precision base, lengths up to 2^64, 26-bits of length-precision (i.e. if length is >2^63 the bottom 37 bits are zero)
    let base: u64 = base_aligned_to_exactly(31) - 1;
    let length: u64 = 0x3FFFFFF_000000000;
    check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;

    Ok(())
}

#[test]
fn inexact_base_exact_length_on_quadrant_boundary() -> Result<(), CapEncodeErr> {
    // Run tests where the base address is (aligned for the quadrant) - 1
    // and the length is just on the representable edge of the quadrant, such that aligning the base requires pushing the length higher to compensate
    // which then realigns the base. 

    // Quadrant 0, 10-bit precision base, lengths up to 2^22, 5-bits of length-precision (i.e. if length is >2^21 the bottom 16 bits are zero)
    let base: u64            = base_aligned_to_exactly(10) - 1; // (aligned to 2^10) - 1
    let length: u64          = 1 << 22;
    let q0 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = align_to(base, 17); // aligned to 2^17
    let expected_length: u64 = (1 << 22) + (1 << 17); // the length has to grow to accomodate the old (base+length), at least it can grow by 1 << 17 once it's in the next quadrant. it can't grow inside quadrant 0.
    assert!(expected_base+expected_length >= base+length);
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q0);

    // Quadrant 1, 17-bit precision base, lengths up to 2^36, 12-bits of length-precision (i.e. if length is >2^35 the bottom 23 bits are zero)
    let base: u64            = base_aligned_to_exactly(17) - 1; // (aligned to 2^17) - 1
    let length: u64          = 1 << 36;
    let q1 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = align_to(base, 24); // aligned to 2^24
    let expected_length: u64 = (1 << 36) + (1 << 24); // the length has to grow to accomodate the old (base+length), at least it can grow by 1 << 24 once it's in the next quadrant. it can't grow inside quadrant 1.
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q1);
    assert!(expected_base+expected_length >= base+length);

    // Quadrant 2, 24-bit precision base, lengths up to 2^50, 19-bits of length-precision (i.e. if length is >2^49 the bottom 30 bits are zero)
    let base: u64            = base_aligned_to_exactly(24) - 1; // (aligned to 2^24) - 1
    let length: u64          = 1 << 50;
    let q2 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = align_to(base, 31); // aligned to 2^31
    let expected_length: u64 = (1 << 50) + (1 << 31); // the length has to grow to accomodate the old (base+length), at least it can grow by 1 << 31 once it's in the next quadrant. it can't grow inside quadrant 2.
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q2);
    assert!(expected_base+expected_length >= base+length);

    // We can't grow out of quadrant 3 because that would require length of 1 << 64 with nonzero base
    // Quadrant 3, 31-bit precision base, lengths up to 2^64, 26-bits of length-precision (i.e. if length is >2^63 the bottom 37 bits are zero)

    Ok(())
}

#[test]
fn exact_base_inexact_length() -> Result<(), CapEncodeErr> {
    // Quadrant 0, 10-bit precision base, lengths up to 2^22, 5-bits of length-precision (i.e. if length is >2^21 the bottom 16 bits are zero)
    let base: u64            = base_aligned_to_exactly(10); // (aligned to 2^10)
    let length: u64          = (1 << 20) + 1;
    let q0 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = base;
    let expected_length: u64 = (1 << 20) + (1 << 15); // length gets rounded up to the nearest block size
    assert!(expected_base+expected_length >= base+length);
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q0);

    // Quadrant 1, 17-bit precision base, lengths up to 2^36, 12-bits of length-precision (i.e. if length is >2^35 the bottom 23 bits are zero)
    let base: u64            = base_aligned_to_exactly(17); // (aligned to 2^17)
    let length: u64          = (1 << 33) + 1;
    let q1 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = base;
    let expected_length: u64 = (1 << 33) + (1 << 21); // length gets rounded up to the nearest block size
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q1);
    assert!(expected_base+expected_length >= base+length);

    // Quadrant 2, 24-bit precision base, lengths up to 2^50, 19-bits of length-precision (i.e. if length is >2^49 the bottom 30 bits are zero)
    let base: u64            = base_aligned_to_exactly(24); // (aligned to 2^24)
    let length: u64          = (1 << 45) + 1;
    let q2 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = base;
    let expected_length: u64 = (1 << 45) + (1 << 26); // length gets rounded up to the nearest block size
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q2);
    assert!(expected_base+expected_length >= base+length);

    let base: u64            = base_aligned_to_exactly(31); // (aligned to 2^31)
    let length: u64          = (1 << 63) + 1;
    let q3 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = base;
    let expected_length: u64 = (1 << 63) + (1 << 37); // length gets rounded up to the nearest block size
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q3);
    assert!(expected_base+expected_length >= base+length);

    Ok(())
    
}

#[test]
fn initially_exact_base_inexact_length_on_quadrant_boundary() -> Result<(), CapEncodeErr> {
    // When length is one over a quadrant boundary, base becomes inexact

    // Quadrant 0, 10-bit precision base, lengths up to 2^22, 5-bits of length-precision (i.e. if length is >2^21 the bottom 16 bits are zero)
    let base: u64            = base_aligned_to_exactly(10); // (aligned to 2^10)
    let length: u64          = (1 << 22) + 1;
    let q0 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = align_to(base, 17); // aligned to 2^17
    let expected_length: u64 = (1 << 22) + (1 << 17); // the length has to grow to accomodate the old (base+length), at least it can grow by 1 << 17 once it's in the next quadrant. it can't grow inside quadrant 0.
    assert!(expected_base+expected_length >= base+length);
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q0);

    // Quadrant 1, 17-bit precision base, lengths up to 2^36, 12-bits of length-precision (i.e. if length is >2^35 the bottom 23 bits are zero)
    let base: u64            = base_aligned_to_exactly(17); // (aligned to 2^17)
    let length: u64          = (1 << 36) + 1;
    let q1 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = align_to(base, 24); // aligned to 2^24
    let expected_length: u64 = (1 << 36) + (1 << 24); // the length has to grow to accomodate the old (base+length), at least it can grow by 1 << 24 once it's in the next quadrant. it can't grow inside quadrant 1.
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q1);
    assert!(expected_base+expected_length >= base+length);

    // Quadrant 2, 24-bit precision base, lengths up to 2^50, 19-bits of length-precision (i.e. if length is >2^49 the bottom 30 bits are zero)
    let base: u64            = base_aligned_to_exactly(24); // (aligned to 2^24)
    let length: u64          = (1 << 50) + 1;
    let q2 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = align_to(base, 31); // aligned to 2^31
    let expected_length: u64 = (1 << 50) + (1 << 31); // the length has to grow to accomodate the old (base+length), at least it can grow by 1 << 31 once it's in the next quadrant. it can't grow inside quadrant 2.
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q2);
    assert!(expected_base+expected_length >= base+length);

    let base: u64            = base_aligned_to_exactly(31); // (aligned to 2^31)
    let length: u64          = (1 << 63) + 1;
    let q3 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = base;
    let expected_length: u64 = (1 << 63) + (1 << 37); // the length has to grow to accomodate the old (base+length), at least it can grow by 1 << 37 once it's in the next quadrant. it can't grow inside quadrant 2.
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q3);
    assert!(expected_base+expected_length >= base+length);

    Ok(())
}

#[test]
fn inexact_base_inexact_length() -> Result<(), CapEncodeErr> {
    // Test the case where the base gets rounded down and the length gets rounded up at the same time.
    // Ideally the length getting rounded up encompasses the base getting rounded down,
    // but in some cases - see the second example here - the length is only a little bit below the block so you need to increment after rounding up.
    // You should only ever need to increment once - in all cases length-blocks >= base-blocks, so one length-block increment should always
    // be enough to compensate for any round-down in the base.

    // Quadrant 0, 10-bit precision base, lengths up to 2^22, 5-bits of length-precision (i.e. if length is >2^21 the bottom 16 bits are zero)

    // If base is one below base-block aligned, by rounding down the base we need to add at least (base-block-length - 1) to the length to compensate
    let base: u64            = base_aligned_to_exactly(10) - 1; // (aligned to 2^10) - 1
    let length: u64          = (1 << 20) + 1;
    let q0 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = base_aligned_to_exactly(10) - (1 << 10); // base gets rounded down to nearest block size
    let expected_length: u64 = (1 << 20) + (1 << 15); // length gets rounded up to the nearest block size, which also encompasses the base rounding down
    assert!(expected_base+expected_length >= base+length);
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q0);

    // If the length is just under one block then it will get rounded up and then incremented as well
    let base: u64            = base_aligned_to_exactly(10) - 1; // (aligned to 2^10) - 1
    let length: u64          = (1 << 20) + (1 << 15) - 1;
    let q0 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = base_aligned_to_exactly(10) - (1 << 10); // base gets rounded down to nearest block size
    let expected_length: u64 = (1 << 20) + (2 << 15); // length gets rounded up to the nearest block size plus one block to account for the base rounding down
    assert!(expected_base+expected_length >= base+length);
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q0);

    // Specical case: base precision == length precision - only possible at smallest length
    let base: u64            = base_aligned_to_exactly(10) - 1; // (aligned to 2^10) - 1
    let length: u64          = (1 << 10) + 1;
    let q0 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = base_aligned_to_exactly(10) - (1 << 10); // base gets rounded down to nearest block size
    let expected_length: u64 = (1 << 10) + (1 << 10); // length gets rounded up to the nearest block size, which also encompasses the base rounding down
    assert!(expected_base+expected_length >= base+length);
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q0);

    // Quadrant 1, 17-bit precision base, lengths up to 2^36, 12-bits of length-precision (i.e. if length is >2^35 the bottom 23 bits are zero)
    let base: u64            = base_aligned_to_exactly(17) - 1; // (aligned to 2^17) - 1
    let length: u64          = (1 << 33) + 1;
    let q1 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = base_aligned_to_exactly(17) - (1 << 17); // base gets rounded down to nearest block size
    let expected_length: u64 = (1 << 33) + (1 << 21); // length gets rounded up to the nearest block size, which also encompasses the base rounding down
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q1);
    assert!(expected_base+expected_length >= base+length);

    // Quadrant 2, 24-bit precision base, lengths up to 2^50, 19-bits of length-precision (i.e. if length is >2^49 the bottom 30 bits are zero)
    let base: u64            = base_aligned_to_exactly(24) - 1; // (aligned to 2^24) - 1
    let length: u64          = (1 << 38) + 1;
    let q2 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = base_aligned_to_exactly(24) - (1 << 24); // base gets rounded down to nearest block size
    let expected_length: u64 = (1 << 38) + (1 << 24); // length gets rounded up to the nearest block size, which also encompasses the base rounding down
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q2);
    assert!(expected_base+expected_length >= base+length);

    let base: u64            = base_aligned_to_exactly(31) - 1; // (aligned to 2^31) - 1
    let length: u64          = (1 << 63) + (1 << 37) - 1;
    let q3 = check_pack_valid_but_not_exact(AddrRange::base_len_64(base, length))?;
    let expected_base: u64   = base_aligned_to_exactly(31) - (1 << 31); // base gets rounded down to nearest block size
    let expected_length: u64 = (1 << 63) + (2 << 37); // length gets rounded up to the nearest block size plus one block to account for the base rounding down
    assert_eq_hex!(AddrRange::base_len_64(expected_base, expected_length), q3);
    assert!(expected_base+expected_length >= base+length);

    Ok(())
}

// What happens if the base (and length) being nonexact pushes the end of the range out of 64-bit?
// 
// To push out of 64-bit base+length needs to move up by as much as possible.
// In q0: 
// let base: u64            = base_aligned_to_exactly(10) - 1; // (aligned to 2^10) - 1
// let length: u64          = (1 << 20) + (1 << 15) - 1; // rounding up gives +1
// let q0 = check_pack_valid_but_not_exact(AddrRange::base_len(base, length))?;
// let expected_base: u64   = base_aligned_to_exactly(10) - (1 << 10); // base gets rounded down to nearest block size
// let expected_length: u64 = (1 << 20) + (2 << 15); // length gets rounded up to the nearest block size plus one block to account for the base rounding down
// overall base+length delta = (-(1 << 10) + 1) + (1 << 15) + 1
// for the initial range to be valid base <= (1 << 64) - (1 << 20) - (1 << 15) + 1
// to get (aligned to 2^10 - 1) take base  = (1 << 64) - (1 << 20) - (1 << 15) - 1
//                                   length= (1 << 20) + (1 << 15) - 1
// new base = (1 << 64) - (1 << 20) - (1 << 15) - (1 << 10)
// which is > (1 << 54) which is the maximum usable base
// => this cannot happen

#[test]
fn inexact_base_exact_length_doesnt_push_out_64bit() {
    // 2024_02 allows a 54-bit base address, 2024_11 requires 54-bit
    let base: u64 = (1 << 54) - (1 << 20) - (1 << 15) - 1;
    let length: u64 = (1 << 20) + (1 << 15) - 1;
    let expanded_past_end = check_pack_valid(AddrRange::base_len_64(base, length)).unwrap();
    let expected_base: u64 = (1 << 54) - (1 << 20) - (1 << 15) - (1 << 10);
    let expected_len: u64 = (1 << 20) + (2 << 15);
    assert_eq!(AddrRange::base_len_64(expected_base, expected_len), expanded_past_end);

    // base and len are 64-bits, but they add up to a 65-bit number
    let expected_end = (expected_base as u128) + (expected_len as u128);
    let expected_end_in_64: Result<u64, _> = expected_end.try_into();
    assert!(expected_end_in_64.is_ok());
}

type Cap = Cap2024_11<Aes128HandrolledSig>;
const SECRET: u128 = 0;
const SECRET2: u128 = 1;

#[test]
fn test_cap_cav1() -> Result<(), CapEncDecErr> {
    let base = base_aligned_to_exactly(24);
    let length = 1 << 30;
    let cap = Cap::new(SECRET, 0, base, length, CapPerms::ReadWrite)?;
    assert_eq!(cap.requested_range()?, (AddrRange::base_len(base, length), CapPerms::ReadWrite));
    cap.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap.check_validity(SECRET2));

    let pow2 = 20;
    let index = 27;
    let cap_cav1 = cap.with_cav1(None, index, pow2)?;
    cap_cav1.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap_cav1.check_validity(SECRET2));
    let expected_cav1_base = base + ((index as u64) << pow2);
    let expected_cav1_length = 1 << pow2;
    assert_eq_hex!(AddrRange::base_len_64(expected_cav1_base, expected_cav1_length), cap_cav1.addr_range);

    let pow2 = 30;
    let index = 0;
    let cap_cav1 = cap.with_cav1(None, index, pow2)?;
    cap_cav1.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap_cav1.check_validity(SECRET2));
    let expected_cav1_base = base + ((index as u64) << pow2);
    let expected_cav1_length = 1 << pow2;
    assert_eq_hex!(AddrRange::base_len_64(expected_cav1_base, expected_cav1_length), cap_cav1.addr_range);

    let pow2 = 29; // 2 elements, 1/2 size
    let index = 2; // one past the end!
    let cav1_res = cap.with_cav1(None, index, pow2);
    assert_eq!(cav1_res, Err(CapEncodeErr::InvalidCaveat));

    let pow2 = 15;
    let index = 1 << 14;
    let cap_cav1 = cap.with_cav1(None, index, pow2)?;
    cap_cav1.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap_cav1.check_validity(SECRET2));
    let expected_cav1_base = base + ((index as u64) << pow2);
    let expected_cav1_length = 1 << pow2;
    assert_eq_hex!(AddrRange::base_len_64(expected_cav1_base, expected_cav1_length), cap_cav1.addr_range);

    // pow2 is too small - size-div is 15 at max
    let pow2 = 14;
    let index = 0;
    let cav1_res = cap.with_cav1(None, index, pow2);
    assert_eq!(cav1_res, Err(CapEncodeErr::UnrepresentableCaveat));

    Ok(())
}

#[test]
fn test_cap_cav2() -> Result<(), CapEncDecErr> {
    let base = base_aligned_to_exactly(24);
    let length = 1 << 30;
    let cap = Cap::new(SECRET, 0, base, length, CapPerms::ReadWrite)?;
    assert_eq!(cap.requested_range()?, (AddrRange::base_len(base, length), CapPerms::ReadWrite));
    cap.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap.check_validity(SECRET2));

    let pow2 = 15;
    let index = 1 << 14;
    let cap_cav1 = cap.with_cav1(None, index, pow2)?;
    cap_cav1.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap_cav1.check_validity(SECRET2));
    let cav1_base = base + ((index as u64) << pow2);
    let cav1_length = 1 << pow2;
    assert_eq_hex!(AddrRange::base_len_64(cav1_base, cav1_length), cap_cav1.addr_range);

    // Shouldn't be able to change cav1
    assert_eq!(cap_cav1.with_cav1(None, index, pow2), Err(CapEncodeErr::NoCaveatsLeft));

    // 2024_02 has 9-bit cav2, 2024_11 has 14-bit
    const CAV2_BASE_POW2: u8 = 15 - 14;

    // identity caveat
    let range_x = 0;
    let range_y = 0x3FFF + 1; // 14 bits + 1
    let cap_cav2 = cap_cav1.with_cav2(None, range_x, range_y)?;
    assert_eq_hex!(cap_cav1.addr_range, cap_cav2.addr_range);
    cap_cav2.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap_cav2.check_validity(SECRET2));

    // non-identity normal caveat
    let range_x = 0x12;
    let range_y = 0x50;
    let cap_cav2 = cap_cav1.with_cav2(None, range_x, range_y)?;
    let cav2_base = cav1_base + ((range_x as u64) << CAV2_BASE_POW2);
    let cav2_end = cav1_base + ((range_y as u64) << CAV2_BASE_POW2);
    assert_eq_hex!(AddrRange::start_end_64(cav2_base, cav2_end), cap_cav2.addr_range);
    cap_cav2.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap_cav2.check_validity(SECRET2));

    // A cap without cav1 can't get cav2
    assert_eq!(cap.with_cav2(None, range_x, range_y), Err(CapEncodeErr::InvalidCaveat));

    // A cap with cav2 can't override cav2
    assert_eq!(cap_cav2.with_cav2(None, range_x, range_y), Err(CapEncodeErr::NoCaveatsLeft));

    // Can't have range_x >= range_y
    let range_x = 0x51;
    let range_y = 0x50;
    assert_eq!(Err(CapEncodeErr::UnrepresentableCaveat), cap_cav1.with_cav2(None, range_x, range_y));

    let range_x = 0x50;
    let range_y = 0x50;
    assert_eq!(Err(CapEncodeErr::UnrepresentableCaveat), cap_cav1.with_cav2(None, range_x, range_y));

    Ok(())
}

#[test]
fn test_cap_perms_chain() -> Result<(), CapEncDecErr> {
    let base = base_aligned_to_exactly(24);
    let length = 1 << 30;
    let cap = Cap::new(SECRET, 0, base, length, CapPerms::ReadWrite)?;
    assert_eq!(CapPermsChain::ReadWrite_0Cav, cap.cap_perms_chain());
    assert_eq!(CapPerms::ReadWrite, cap.cap_perms());
    cap.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap.check_validity(SECRET2));

    // Shrink to read at cav1
    let cap_cav1 = cap.with_cav1(Some(CapPerms::Read), 0, 30)?;
    assert_eq!(CapPermsChain::ShrunkToRead_1Cav, cap_cav1.cap_perms_chain());
    assert_eq!(CapPerms::Read, cap_cav1.cap_perms());
    cap_cav1.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap_cav1.check_validity(SECRET2));

    let cap_cav2 = cap_cav1.with_cav2(None, 0, 0x1FF + 1)?;
    assert_eq!(CapPermsChain::ShrunkToRead_2Cav_ByCav1, cap_cav2.cap_perms_chain());
    assert_eq!(CapPerms::Read, cap_cav2.cap_perms());
    cap_cav2.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap_cav2.check_validity(SECRET2));

    // Can't shrink cap_cav1 to write
    assert_eq!(Err(CapEncodeErr::CantShrinkPerms(CapPerms::Write)), cap_cav1.with_cav2(Some(CapPerms::Write), 0, 0x1FF + 1));


    // shrink to write at cav2
    let cap_cav1 = cap.with_cav1(None, 0, 30)?;
    assert_eq!(CapPermsChain::ReadWrite_1Cav, cap_cav1.cap_perms_chain());
    assert_eq!(CapPerms::ReadWrite, cap_cav1.cap_perms());
    cap_cav1.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap_cav1.check_validity(SECRET2));

    let cap_cav2 = cap_cav1.with_cav2(Some(CapPerms::Write), 0, 0x1FF + 1)?;
    assert_eq!(CapPermsChain::ShrunkToWrite_2Cav_ByCav2, cap_cav2.cap_perms_chain());
    assert_eq!(CapPerms::Write, cap_cav2.cap_perms());
    cap_cav2.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap_cav2.check_validity(SECRET2));

    // Start as write-only
    let cap = Cap::new(SECRET, 0, base, length, CapPerms::Write)?;
    assert_eq!(CapPermsChain::WriteOnly_0Cav, cap.cap_perms_chain());
    assert_eq!(CapPerms::Write, cap.cap_perms());
    cap.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap.check_validity(SECRET2));

    let cap_cav1 = cap.with_cav1(None, 0, 30)?;
    assert_eq!(CapPermsChain::WriteOnly_1Cav, cap_cav1.cap_perms_chain());
    assert_eq!(CapPerms::Write, cap_cav1.cap_perms());
    cap_cav1.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap_cav1.check_validity(SECRET2));

    let cap_cav2 = cap_cav1.with_cav2(None, 0, 0x1FF + 1)?;
    assert_eq!(CapPermsChain::WriteOnly_2Cav, cap_cav2.cap_perms_chain());
    assert_eq!(CapPerms::Write, cap_cav2.cap_perms());
    cap_cav2.check_validity(SECRET).unwrap();
    assert_eq!(Err(CapDecodeErr::InvalidSignature), cap_cav2.check_validity(SECRET2));

    // Can't shrink cap to read with cav1
    assert_eq!(Err(CapEncodeErr::CantShrinkPerms(CapPerms::Read)), cap.with_cav1(Some(CapPerms::Read), 0, 30));
    
    // Can't shrink cap_cav1 to read with cav2
    assert_eq!(Err(CapEncodeErr::CantShrinkPerms(CapPerms::Read)), cap_cav1.with_cav2(Some(CapPerms::Read), 0, 0x1FF + 1));

    Ok(())
}