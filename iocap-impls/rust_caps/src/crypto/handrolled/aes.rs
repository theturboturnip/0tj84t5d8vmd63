//! Handrolled AES-128 implementation.
//! Based on the definitions in [https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf]
//!
//! TODO read up on current block cipher modes of operation - which one is best for me? https://csrc.nist.gov/projects/block-cipher-techniques/bcm/current-modes
//! In Recommendation for block cipher modes of operation:
//! - Electronic Codebook (ECB)
//! - Cipher Block Chaining (CBC)
//! - Cipher Feedback (CFB)
//! - Output Feedback (OFB)
//! - Counter (CTR)
//!
//! For our usage of AES-128 the plaintext is always 1x (or 2x?) the block size i.e. 128bits.
//! These all concern how a stream of many blocks are hashed, so it doesn't matter for us. We can just use ECB.
//! "In the ECB mode, under a given key, any given plaintext block always gets encrypted to the same ciphertext block.
//! If this property is undesirable in a particular application, the ECB mode should not be used."
//!
//! Also has an Authentication Mode CMAC: https://csrc.nist.gov/pubs/sp/800/38/b/upd1/final
//! "CMAC is designed to detect intentional, unauthorized modifications of the data, as well as accidental modifications."
//! I... don't think we need that. See [crate::crypto::Signaturer] - we just need to prove the previous signature/secret was known without exposing it.
//! Particularly given that the message is public...?
//! https://security.stackexchange.com/a/119613 - tampering with ciphertext can affect the decrypted plaintext,
//! but given that the plaintext is included with the signature it's *less likely*? *impossible*? that an attacker
//! can manipulate both the ciphertext and plaintext to their advantage without knowing the key.
//!
//! We are essentially constantly subjecting ourselves to known plaintext attacks and differential analysis attacks.
//! https://www.cs.clemson.edu/course/cpsc424/material/Cryptography/Attacks%20on%20Symmetric%20Key.pdf
//! This stackoverflow answer claims AES protects against known-plaintext due to the sbox: https://crypto.stackexchange.com/a/64079
//! Apparently AES is vulnerable to differential-fault analysis? https://eprint.iacr.org/2003/010.pdf
//! Example of chosen-plaintext attack on AES when using predictable IVs: https://derekwill.com/2021/01/01/aes-cbc-mode-chosen-plaintext-attack/
//!     this attack is about submitting arbitrary plaintexts to the oracle which use XOR to try and recreate a specific message with a specific IV. It reveals the message, not the key.
//! Differential analysis of round-reduced aes faulty ciphertexts https://www.emse.fr/~dutertre/doc_recherche/P_2013_9_talk_RMA_DFTS2013.pdf
//!     this is about inducing skipped rounds in a HW implementation using injected faults and inspecting the ciphertext
//! Differential Fault Analysis of the Advanced Encryption Standard using a Single Fault  https://eprint.iacr.org/2009/575.pdf
//!     "We demonstrate that when a single random byte fault is induced at the input of the eighth round, the AES key can be deduced using a two
//! stage algorithm. The first step has a statistical expectation of reducing the possible key hypotheses to 2^32, and the second step to a mere 2^8."
//!     fault induction -> eh
//! Differential and Linear Cryptanalysis in Evaluating AES Candidate Algorithms https://www.cs.cmu.edu/~hannes/diffLinAES.pdf
//!     This paper was written before the winning candidate for AES was selected, and states "virtually all AES candidates are ciphers designed by cryptanalysis and hence exhibit strong security properties"
//!     The selected candidate was Rinjandel
//!     Rinjandel was a frontrunner in their minds but all that really said was it "resists all security concerns so far".
//! https://crypto.stackexchange.com/questions/103349/best-complexity-of-guessing-difference-of-aes-256-outputs
//!     I'm personally happy with saying AES is resistant to differential analysis sans faults, but need to consult a crypto person to figure this out fr.

use crate::{checked_bits::ConstCompare, crypto::handrolled::galois::GaloisFieldByte};

use super::galois::GaloisFieldWord;

/// Column-major
type AesState = [GaloisFieldWord; 4];

fn reshape_words_to_states<const N_W: usize, const N_S: usize>(arr: [GaloisFieldWord; N_W]) -> [AesState; N_S] {
    let _ = ConstCompare::<N_S, N_W>::A_EQ_B_DIV_4;
    let mut out = [AesState::default(); N_S];
    for i in 0..N_S {
        out[i] = [
            arr[i * 4 + 0],
            arr[i * 4 + 1],
            arr[i * 4 + 2],
            arr[i * 4 + 3],
        ];
    }
    out
}

/// "The block cipher specified in this standard with 128-bit keys"
pub fn aes128(data_in: u128, key: u128) -> u128 {
    let w = key_expansion_aes128(key);
    let out = cipher::<10>(u128_to_aes_state(data_in), &w);
    aes_state_to_u128(out)
}

pub fn aes128_inv(data_in: u128, key: u128) -> u128 {
    let w = key_expansion_aes128(key);
    let out = inv_cipher::<10>(u128_to_aes_state(data_in), &w);
    aes_state_to_u128(out)
}

// pub fn aes128_eq_inv(data_in: u128, key: u128) -> u128 {
//     let dw = {
//         let mut dw = [AesState::default(); 11];
//         key_expansion_eic::<4, 10>(&u128_to_aes_state(key), &mut dw);
//         dw
//     };
//     let out = eq_inv_cipher::<10>(u128_to_aes_state(data_in), &dw);
//     aes_state_to_u128(out)
// }

pub fn aes128_half_and_half(plain: u128, cipher: u128, key: u128) -> bool {
    let w = key_expansion_aes128(key);
    half_and_half_plain_encrypts_to_cipher::<10>(
        u128_to_aes_state(plain),
        u128_to_aes_state(cipher),
        &w,
    )
}

fn u128_to_aes_state(x: u128) -> AesState {
    let bs = x.to_le_bytes();
    [
        GaloisFieldWord::from_byte_slice(&bs[0..4]),
        GaloisFieldWord::from_byte_slice(&bs[4..8]),
        GaloisFieldWord::from_byte_slice(&bs[8..12]),
        GaloisFieldWord::from_byte_slice(&bs[12..16]),
    ]
}

fn aes_state_to_u128(s: AesState) -> u128 {
    let bs = [
        s[0][0].into(),
        s[0][1].into(),
        s[0][2].into(),
        s[0][3].into(),
        s[1][0].into(),
        s[1][1].into(),
        s[1][2].into(),
        s[1][3].into(),
        s[2][0].into(),
        s[2][1].into(),
        s[2][2].into(),
        s[2][3].into(),
        s[3][0].into(),
        s[3][1].into(),
        s[3][2].into(),
        s[3][3].into(),
    ];
    u128::from_le_bytes(bs)
}

/// My personal function - presumably this has been discovered before
fn half_and_half_plain_encrypts_to_cipher<const N_ROUNDS: usize>(
    plain: AesState,
    cipher: AesState,
    w: &[AesState],
) -> bool {
    assert!(N_ROUNDS % 2 == 0);
    // Use a half-and-half strategy - do half an AES-enc on plain, half an AES-dec on cipher, and meet in the middle
    // superior for hardware checking, only need N_ROUNDS/2 actual rounds becasue the encryption and decryption can be done in parallel

    let mut s_p = plain;
    for round in 0..N_ROUNDS / 2 {
        // EXCLUSIVE => for N=10 round = {0, 1, 2, 3, 4}
        s_p = add_round_key(s_p, &w[round]);
        s_p = sub_bytes(s_p);
        s_p = shift_rows(s_p);
        s_p = mix_columns(s_p);
    }

    let mut s_c = cipher;
    s_c = add_round_key(s_c, &w[N_ROUNDS]);
    s_c = inv_shift_rows(s_c);
    s_c = inv_sub_bytes(s_c);
    for round in ((1 + N_ROUNDS / 2)..N_ROUNDS).rev() {
        // EXCLUSIVE => for N=10 round = {9, 8, 7, 6}
        s_c = add_round_key(s_c, &w[round]);
        s_c = inv_mix_columns(s_c);
        s_c = inv_shift_rows(s_c);
        s_c = inv_sub_bytes(s_c);
    }
    // Need to do this for one of them, choose this one so the number of operations is the same on both sides
    s_c = add_round_key(s_c, &w[N_ROUNDS / 2]);

    s_p == s_c
}

/// "The transformation of blocks that underlies AES. The key schedule and number of rounds are parameters of the transformation"
///
/// w = key schedule
///
/// See section 5.0
fn cipher<const N_ROUNDS: usize>(data_in: AesState, w: &[AesState]) -> AesState {
    let mut s = data_in;
    s = add_round_key(s, &w[0]);
    for round in 1..N_ROUNDS {
        s = sub_bytes(s);
        s = shift_rows(s);
        s = mix_columns(s);
        s = add_round_key(s, &w[round]);
    }
    s = sub_bytes(s);
    s = shift_rows(s);
    s = add_round_key(s, &w[N_ROUNDS]);

    s
}

fn inv_cipher<const N_ROUNDS: usize>(data_in: AesState, w: &[AesState]) -> AesState {
    let mut s = data_in;
    s = add_round_key(s, &w[N_ROUNDS]);
    for round in (1..N_ROUNDS).rev() {
        s = inv_shift_rows(s);
        s = inv_sub_bytes(s);
        s = add_round_key(s, &w[round]);
        s = inv_mix_columns(s);
    }
    s = inv_shift_rows(s);
    s = inv_sub_bytes(s);
    s = add_round_key(s, &w[0]);

    s
}

// /// TODO what's the difference between inv_cipher and eq_inv_cipher
// fn eq_inv_cipher<const N_ROUNDS: usize>(data_in: AesState, dw: &[AesState]) -> AesState {
//     let mut s = data_in;
//     s = add_round_key(s, &dw[N_ROUNDS]);
//     for round in (1..N_ROUNDS).rev() {
//         s = inv_sub_bytes(s);
//         s = inv_shift_rows(s);
//         s = inv_mix_columns(s);
//         s = add_round_key(s, &dw[round]);
//     }
//     s = inv_sub_bytes(s);
//     s = inv_shift_rows(s);
//     s = add_round_key(s, &dw[0]);

//     s
// }

/// "The transformation of the state in which a round key is combined with the state"
///
/// This is the inverse of itself, becaise Galois field addition is XOR.
/// i.e. x + y + y = x for all x, y
fn add_round_key(s: AesState, ws: &AesState) -> AesState {
    [s[0] + ws[0], s[1] + ws[1], s[2] + ws[2], s[3] + ws[3]]
}

/// "The transformation of the state that takes all of the columns of the state and mixes their data (independently of one another) to produce new columns"
///
/// See section 4.3, 5.1.3
fn mix_columns(s: AesState) -> AesState {
    const GF02: GaloisFieldByte = GaloisFieldByte(0x2);
    const GF03: GaloisFieldByte = GaloisFieldByte(0x3);

    let mix_column = |w: GaloisFieldWord| -> GaloisFieldWord {
        GaloisFieldWord([
            (GF02 * w[0]) + (GF03 * w[1]) + w[2] + w[3],
            w[0] + (GF02 * w[1]) + (GF03 * w[2]) + w[3],
            w[0] + w[1] + (GF02 * w[2]) + (GF03 * w[3]),
            (GF03 * w[0]) + w[1] + w[2] + (GF02 * w[3]),
        ])
    };

    [
        mix_column(s[0]),
        mix_column(s[1]),
        mix_column(s[2]),
        mix_column(s[3]),
    ]
}

/// "The inverse of mix_columns"
///
/// See section 5.3.3
fn inv_mix_columns(s: AesState) -> AesState {
    const GF0E: GaloisFieldByte = GaloisFieldByte(0xE);
    const GF0B: GaloisFieldByte = GaloisFieldByte(0xB);
    const GF0D: GaloisFieldByte = GaloisFieldByte(0xD);
    const GF09: GaloisFieldByte = GaloisFieldByte(0x9);

    let inv_mix_column = |w: GaloisFieldWord| -> GaloisFieldWord {
        GaloisFieldWord([
            (GF0E * w[0]) + (GF0B * w[1]) + (GF0D * w[2]) + (GF09 * w[3]),
            (GF09 * w[0]) + (GF0E * w[1]) + (GF0B * w[2]) + (GF0D * w[3]),
            (GF0D * w[0]) + (GF09 * w[1]) + (GF0E * w[2]) + (GF0B * w[3]),
            (GF0B * w[0]) + (GF0D * w[1]) + (GF09 * w[2]) + (GF0E * w[3]),
        ])
    };

    [
        inv_mix_column(s[0]),
        inv_mix_column(s[1]),
        inv_mix_column(s[2]),
        inv_mix_column(s[3]),
    ]
}

/// "The routine that generates the round keys from the key"
fn key_expansion<const N_KEY_WORDS: usize, const N_ROUNDS: usize>(
    key: &[GaloisFieldWord; N_KEY_WORDS],
    w: &mut [GaloisFieldWord]
) {
    const RCON: [GaloisFieldWord; 10] = [
        GaloisFieldWord::from_bytes([0x01, 0, 0, 0]),
        GaloisFieldWord::from_bytes([0x02, 0, 0, 0]),
        GaloisFieldWord::from_bytes([0x04, 0, 0, 0]),
        GaloisFieldWord::from_bytes([0x08, 0, 0, 0]),
        GaloisFieldWord::from_bytes([0x10, 0, 0, 0]),
        GaloisFieldWord::from_bytes([0x20, 0, 0, 0]),
        GaloisFieldWord::from_bytes([0x40, 0, 0, 0]),
        GaloisFieldWord::from_bytes([0x80, 0, 0, 0]),
        GaloisFieldWord::from_bytes([0x1b, 0, 0, 0]),
        GaloisFieldWord::from_bytes([0x36, 0, 0, 0]),
    ];

    // Section 5.2


    // The first Nk words of the expanded key are the key itself.
    for i in 0..key.len() {
        w[i] = key[i]
    }
    for i in N_KEY_WORDS..=(4 * N_ROUNDS + 3) {

        let mut temp = w[i - 1];
        if i % N_KEY_WORDS == 0 {
            // Use -1 here because RCON is one-indexed in the definition
            temp = sub_word(rot_word(temp)) + RCON[(i / N_KEY_WORDS) - 1];
        } else if N_KEY_WORDS > 6 && i % N_KEY_WORDS == 4 {
            temp = sub_word(temp);
        }
        w[i] = w[i - N_KEY_WORDS] + temp;
    }
}

// /// "The routine that generates the modified round keys for the equivalent inverse cipher"
// fn key_expansion_eic<const N_KEY_WORDS: usize, const N_ROUNDS: usize>(
//     key: &[GaloisFieldWord; N_KEY_WORDS],
//     storage: &mut [[GaloisFieldWord; N_KEY_WORDS]],
// ) {
//     // Comparing algorithm 2 (key_expansion) to 5 (key_expansion_eic) we can find that key_expansion_eic is the same as key_expansion except you run inv_mix_columns on some of the keys

//     key_expansion::<N_KEY_WORDS, N_ROUNDS>(key, storage);
//     storage.iter_mut()
//         .enumerate()
//         .map(|(i, k)| {
//             if i > 0 && i < N_ROUNDS {
//                 *k = inv_mix_columns(*k)
//             }
//         })
//         .count();
// }

fn key_expansion_aes128(    key: u128,
) -> [AesState; 11] {
    let mut w = [GaloisFieldWord::default(); 44];
    key_expansion::<4, 10>(&u128_to_aes_state(key), &mut w);
    reshape_words_to_states(w)
}

/// "The transformation of words in which the four bytes of the word are permuted cyclically"
fn rot_word(w: GaloisFieldWord) -> GaloisFieldWord {
    GaloisFieldWord([w[1], w[2], w[3], w[0]])
}

/// "The transformation of the state in which the last three rows are cyclically shifted by different offsets"
///
/// Section 5.1.2
fn shift_rows(s: AesState) -> AesState {
    // Transposed from diagram in figure 3, because it's column major,
    // and the indices are the other way around - s is column-major => indexed [c][r] instead of r,c
    [
        GaloisFieldWord([s[0][0], s[1][1], s[2][2], s[3][3]]),
        GaloisFieldWord([s[1][0], s[2][1], s[3][2], s[0][3]]),
        GaloisFieldWord([s[2][0], s[3][1], s[0][2], s[1][3]]),
        GaloisFieldWord([s[3][0], s[0][1], s[1][2], s[2][3]]),
    ]
}

/// "The inverse of shift_rows"
///
/// Section 5.3.1
fn inv_shift_rows(s: AesState) -> AesState {
    // Transposed from diagram in figure 9, because it's column major,
    // and the indices are the other way around - s is column-major => indexed [c][r] instead of r,c

    // Transposed with row-major indexing:
    // s00 s13 s22 s31
    // s01 s10 s23 s32
    // s02 s11 s20 s33
    // s03 s12 s21 s30

    // Transposed with col-major indexing:
    // s00 s31 s22 s13
    // s10 s01 s32 s23
    // s20 s11 s02 s33
    // s30 s21 s12 s03

    [
        GaloisFieldWord([s[0][0], s[3][1], s[2][2], s[1][3]]),
        GaloisFieldWord([s[1][0], s[0][1], s[3][2], s[2][3]]),
        GaloisFieldWord([s[2][0], s[1][1], s[0][2], s[3][3]]),
        GaloisFieldWord([s[3][0], s[2][1], s[1][2], s[0][3]]),
    ]
}

/// "The transformation of bytes defined by the S-box"
///
/// SBox values taken from Table 4
fn sbox(sb: GaloisFieldByte) -> GaloisFieldByte {
    #[rustfmt::skip]
    const SBOX: [u8; 256] = [
        0x63,0x7c,0x77,0x7b,0xf2,0x6b,0x6f,0xc5,0x30,0x01,0x67,0x2b,0xfe,0xd7,0xab,0x76,
        0xca,0x82,0xc9,0x7d,0xfa,0x59,0x47,0xf0,0xad,0xd4,0xa2,0xaf,0x9c,0xa4,0x72,0xc0,
        0xb7,0xfd,0x93,0x26,0x36,0x3f,0xf7,0xcc,0x34,0xa5,0xe5,0xf1,0x71,0xd8,0x31,0x15,
        0x04,0xc7,0x23,0xc3,0x18,0x96,0x05,0x9a,0x07,0x12,0x80,0xe2,0xeb,0x27,0xb2,0x75,
        0x09,0x83,0x2c,0x1a,0x1b,0x6e,0x5a,0xa0,0x52,0x3b,0xd6,0xb3,0x29,0xe3,0x2f,0x84,
        0x53,0xd1,0x00,0xed,0x20,0xfc,0xb1,0x5b,0x6a,0xcb,0xbe,0x39,0x4a,0x4c,0x58,0xcf,
        0xd0,0xef,0xaa,0xfb,0x43,0x4d,0x33,0x85,0x45,0xf9,0x02,0x7f,0x50,0x3c,0x9f,0xa8,
        0x51,0xa3,0x40,0x8f,0x92,0x9d,0x38,0xf5,0xbc,0xb6,0xda,0x21,0x10,0xff,0xf3,0xd2,
        0xcd,0x0c,0x13,0xec,0x5f,0x97,0x44,0x17,0xc4,0xa7,0x7e,0x3d,0x64,0x5d,0x19,0x73,
        0x60,0x81,0x4f,0xdc,0x22,0x2a,0x90,0x88,0x46,0xee,0xb8,0x14,0xde,0x5e,0x0b,0xdb,
        0xe0,0x32,0x3a,0x0a,0x49,0x06,0x24,0x5c,0xc2,0xd3,0xac,0x62,0x91,0x95,0xe4,0x79,
        0xe7,0xc8,0x37,0x6d,0x8d,0xd5,0x4e,0xa9,0x6c,0x56,0xf4,0xea,0x65,0x7a,0xae,0x08,
        0xba,0x78,0x25,0x2e,0x1c,0xa6,0xb4,0xc6,0xe8,0xdd,0x74,0x1f,0x4b,0xbd,0x8b,0x8a,
        0x70,0x3e,0xb5,0x66,0x48,0x03,0xf6,0x0e,0x61,0x35,0x57,0xb9,0x86,0xc1,0x1d,0x9e,
        0xe1,0xf8,0x98,0x11,0x69,0xd9,0x8e,0x94,0x9b,0x1e,0x87,0xe9,0xce,0x55,0x28,0xdf,
        0x8c,0xa1,0x89,0x0d,0xbf,0xe6,0x42,0x68,0x41,0x99,0x2d,0x0f,0xb0,0x54,0xbb,0x16
    ];
    GaloisFieldByte(SBOX[sb.0 as usize])
}

/// "The inverse of sbox()"
///
/// Inv Sbox values taken from Table 6
fn inv_sbox(sb: GaloisFieldByte) -> GaloisFieldByte {
    #[rustfmt::skip]
    const INV_SBOX: [u8; 256] = [
        0x52,0x09,0x6a,0xd5,0x30,0x36,0xa5,0x38,0xbf,0x40,0xa3,0x9e,0x81,0xf3,0xd7,0xfb,
        0x7c,0xe3,0x39,0x82,0x9b,0x2f,0xff,0x87,0x34,0x8e,0x43,0x44,0xc4,0xde,0xe9,0xcb,
        0x54,0x7b,0x94,0x32,0xa6,0xc2,0x23,0x3d,0xee,0x4c,0x95,0x0b,0x42,0xfa,0xc3,0x4e,
        0x08,0x2e,0xa1,0x66,0x28,0xd9,0x24,0xb2,0x76,0x5b,0xa2,0x49,0x6d,0x8b,0xd1,0x25,
        0x72,0xf8,0xf6,0x64,0x86,0x68,0x98,0x16,0xd4,0xa4,0x5c,0xcc,0x5d,0x65,0xb6,0x92,
        0x6c,0x70,0x48,0x50,0xfd,0xed,0xb9,0xda,0x5e,0x15,0x46,0x57,0xa7,0x8d,0x9d,0x84,
        0x90,0xd8,0xab,0x00,0x8c,0xbc,0xd3,0x0a,0xf7,0xe4,0x58,0x05,0xb8,0xb3,0x45,0x06,
        0xd0,0x2c,0x1e,0x8f,0xca,0x3f,0x0f,0x02,0xc1,0xaf,0xbd,0x03,0x01,0x13,0x8a,0x6b,
        0x3a,0x91,0x11,0x41,0x4f,0x67,0xdc,0xea,0x97,0xf2,0xcf,0xce,0xf0,0xb4,0xe6,0x73,
        0x96,0xac,0x74,0x22,0xe7,0xad,0x35,0x85,0xe2,0xf9,0x37,0xe8,0x1c,0x75,0xdf,0x6e,
        0x47,0xf1,0x1a,0x71,0x1d,0x29,0xc5,0x89,0x6f,0xb7,0x62,0x0e,0xaa,0x18,0xbe,0x1b,
        0xfc,0x56,0x3e,0x4b,0xc6,0xd2,0x79,0x20,0x9a,0xdb,0xc0,0xfe,0x78,0xcd,0x5a,0xf4,
        0x1f,0xdd,0xa8,0x33,0x88,0x07,0xc7,0x31,0xb1,0x12,0x10,0x59,0x27,0x80,0xec,0x5f,
        0x60,0x51,0x7f,0xa9,0x19,0xb5,0x4a,0x0d,0x2d,0xe5,0x7a,0x9f,0x93,0xc9,0x9c,0xef,
        0xa0,0xe0,0x3b,0x4d,0xae,0x2a,0xf5,0xb0,0xc8,0xeb,0xbb,0x3c,0x83,0x53,0x99,0x61,
        0x17,0x2b,0x04,0x7e,0xba,0x77,0xd6,0x26,0xe1,0x69,0x14,0x63,0x55,0x21,0x0c,0x7d
    ];

    GaloisFieldByte(INV_SBOX[sb.0 as usize])
}

/// "The transformation of the state that applies the S-box independently to each byte of the state"
fn sub_bytes(s: AesState) -> AesState {
    [
        sub_word(s[0]),
        sub_word(s[1]),
        sub_word(s[2]),
        sub_word(s[3]),
    ]
}

/// "The inverse of sub_bytes"
fn inv_sub_bytes(s: AesState) -> AesState {
    [
        inv_sub_word(s[0]),
        inv_sub_word(s[1]),
        inv_sub_word(s[2]),
        inv_sub_word(s[3]),
    ]
}

/// "The transformation of words in which the S-box is applied to each of the four bytes of the word"
fn sub_word(w: GaloisFieldWord) -> GaloisFieldWord {
    GaloisFieldWord(w.0.map(sbox))
}

/// The inverse of sub_word - not actually defined in the FIPS paper but used here for convenience
fn inv_sub_word(w: GaloisFieldWord) -> GaloisFieldWord {
    GaloisFieldWord(w.0.map(inv_sbox))
}

#[cfg(all(test, feature="std"))]
mod test {
    use assert_hex::assert_eq_hex;

    use crate::crypto::handrolled::aes::*;
    use crate::crypto::handrolled::galois::*;

    // FIPS-197 literals are expressed in reverse byte order.
    // The "first byte" of the AES state, i.e. state[0], is presented first,
    // but when expressed in literal form that byte is the *top* byte.
    // Literals from FIPS-197 need to be byte-swapped.
    use crate::util::le_literal;

    // The "first byte" of the AES word, state[0], is presented first as byte #3
    const fn fips_aes_word(l: u32) -> GaloisFieldWord {
        GaloisFieldWord::from_bytes(l.to_be_bytes())
    }

    #[test]
    fn test_aes128() {
        // From appendix B, equivalent to test_cipher but using u128 to store things.
        const DATA_IN: u128 = le_literal(0x3243f6a8_885a308d_313198a2_e0370734);
        const KEY: u128 = le_literal(0x2b7e1516_28aed2a6_abf71588_09cf4f3c);
        const EXPECTED_DATA_OUT: u128 = le_literal(0x3925841d_02dc09fb_dc118597_196a0b32);

        let data_out = aes128(DATA_IN, KEY);

        assert_eq_hex!(data_out, EXPECTED_DATA_OUT);
    }

    #[test]
    fn test_aes128_inv() {
        // From appendix B, equivalent to test_cipher but using u128 to store things.
        const EXPECTED_DATA_IN: u128 = le_literal(0x3243f6a8_885a308d_313198a2_e0370734);
        const KEY: u128 = le_literal(0x2b7e1516_28aed2a6_abf71588_09cf4f3c);
        const DATA_OUT: u128 = le_literal(0x3925841d_02dc09fb_dc118597_196a0b32);

        let data_in = aes128_inv(DATA_OUT, KEY);

        assert_eq_hex!(data_in, EXPECTED_DATA_IN);
    }

    // #[test]
    // fn test_aes128_eq_inv() {
    //     // From appendix B, equivalent to test_cipher but using u128 to store things.
    //     const EXPECTED_DATA_IN: u128 = le_literal(0x3243f6a8_885a308d_313198a2_e0370734);
    //     const KEY: u128 = le_literal(0x2b7e1516_28aed2a6_abf71588_09cf4f3c);
    //     const DATA_OUT: u128 = le_literal(0x3925841d_02dc09fb_dc118597_196a0b32);

    //     let data_in = aes128_eq_inv(DATA_OUT, KEY);

    //     assert_eq_hex!(data_in, EXPECTED_DATA_IN);
    // }

    #[test]
    fn test_half_and_half() {
        // From appendix B, equivalent to test_cipher but using u128 to store things.
        const DATA_IN: u128 = le_literal(0x3243f6a8_885a308d_313198a2_e0370734);
        const KEY: u128 = le_literal(0x2b7e1516_28aed2a6_abf71588_09cf4f3c);
        const DATA_OUT: u128 = le_literal(0x3925841d_02dc09fb_dc118597_196a0b32);

        assert!(aes128_half_and_half(DATA_IN, DATA_OUT, KEY));
    }

    #[test]
    fn test_cipher() {
        // From Appendix B
        let data_in: AesState = [
            fips_aes_word(0x3243f6a8),
            fips_aes_word(0x885a308d),
            fips_aes_word(0x313198a2),
            fips_aes_word(0xe0370734),
        ];
        let key: [GaloisFieldWord; 4] = [
            fips_aes_word(0x2b7e1516),
            fips_aes_word(0x28aed2a6),
            fips_aes_word(0xabf71588),
            fips_aes_word(0x09cf4f3c),
        ];
        let expected_data_out: AesState = [
            fips_aes_word(0x3925841d),
            fips_aes_word(0x02dc09fb),
            fips_aes_word(0xdc118597),
            fips_aes_word(0x196a0b32),
        ];

        let expanded_keys = key_expansion_aes128(aes_state_to_u128(key));
        let data_out = cipher::<10>(data_in, &expanded_keys);
        assert_eq_hex!(data_out, expected_data_out);
    }

    #[test]
    fn test_key_expansion() {
        // From appx A.1

        const INITIAL_KEY: [GaloisFieldWord; 4] = [
            GaloisFieldWord::from_bytes([0x2b, 0x7e, 0x15, 0x16]),
            GaloisFieldWord::from_bytes([0x28, 0xae, 0xd2, 0xa6]),
            GaloisFieldWord::from_bytes([0xab, 0xf7, 0x15, 0x88]),
            GaloisFieldWord::from_bytes([0x09, 0xcf, 0x4f, 0x3c]),
        ];

        #[rustfmt::skip]
        const EXPECTED_U128_KEYS: [u128; 11] = [
            // Initial key
            le_literal(0x2b7e1516_28aed2a6_abf71588_09cf4f3c),
            // expansions
            le_literal(0xa0fafe17_88542cb1_23a33939_2a6c7605),
            le_literal(0xf2c295f2_7a96b943_5935807a_7359f67f),
            le_literal(0x3d80477d_4716fe3e_1e237e44_6d7a883b),
            le_literal(0xef44a541_a8525b7f_b671253b_db0bad00),
            le_literal(0xd4d1c6f8_7c839d87_caf2b8bc_11f915bc),
            le_literal(0x6d88a37a_110b3efd_dbf98641_ca0093fd),
            le_literal(0x4e54f70e_5f5fc9f3_84a64fb2_4ea6dc4f),
            le_literal(0xead27321_b58dbad2_312bf560_7f8d292f),
            le_literal(0xac7766f3_19fadc21_28d12941_575c006e),
            le_literal(0xd014f9a8_c9ee2589_e13f0cc8_b6630ca6),
        ];

        let expanded_keys = key_expansion_aes128(aes_state_to_u128(INITIAL_KEY));
        let expanded_u128s: Vec<u128> = expanded_keys
            .into_iter()
            .map(|w| -> u128 { aes_state_to_u128(w) })
            .collect();

        assert_eq_hex!(expanded_u128s, EXPECTED_U128_KEYS);
    }

    #[test]
    fn fuzz_10000x_vs_openssl_ige() {
        // OpenSSL IGE Infinite Garble Extension provides an API similar to plain AES 128.
        use crate::util::byte_array_to_u128;
        use openssl::aes::{aes_ige, AesKey};
        use openssl::rand::rand_bytes;

        for _ in 0..10000 {
            let mut input_bytes = [0; 16];
            let mut key_bytes = [0; 16];
            rand_bytes(&mut input_bytes).unwrap();
            rand_bytes(&mut key_bytes).unwrap();

            let openssl_key = AesKey::new_encrypt(&key_bytes).unwrap();
            let mut openssl_out_bytes = [0; 16];
            let mut fake_openssl_iv = [0; 32]; // don't know why this is mut or 32
            aes_ige(
                &input_bytes,
                &mut openssl_out_bytes,
                &openssl_key,
                &mut fake_openssl_iv,
                openssl::symm::Mode::Encrypt,
            );
            let openssl_output_u128 = byte_array_to_u128(&openssl_out_bytes);

            let input_u128 = byte_array_to_u128(&input_bytes);
            let key_u128 = byte_array_to_u128(&key_bytes);
            let handrolled_output_u128 = aes128(input_u128, key_u128);

            assert_eq_hex!(handrolled_output_u128, openssl_output_u128);
        }
    }

    #[test]
    fn fuzz_10000x_vs_openssl_ecb() {
        // OpenSSL AES-128-ECB (Electronic Code Book) should be exactly equivalent to plain AES-128 - ECB mode has no IV.
        use crate::util::byte_array_to_u128;
        use crate::util::byte_slice_to_u128;
        use openssl::rand::rand_bytes;
        use openssl::symm::Cipher;
        use openssl::symm::Crypter;

        for _ in 0..10000 {
            let mut input_bytes = [0; 16];
            let mut key_bytes = [0; 16];
            rand_bytes(&mut input_bytes).unwrap();
            rand_bytes(&mut key_bytes).unwrap();

            let mut crypter = Crypter::new(
                Cipher::aes_128_ecb(),
                openssl::symm::Mode::Encrypt,
                &key_bytes,
                None,
            )
            .unwrap();
            crypter.pad(false);
            let mut openssl_result_bytes = [0; 32];
            let mut count = crypter
                .update(&input_bytes, &mut openssl_result_bytes)
                .unwrap();
            count += crypter.finalize(&mut openssl_result_bytes).unwrap();
            assert_eq!(count, 16);
            let openssl_output_u128 = byte_slice_to_u128(&openssl_result_bytes[0..count]).unwrap();

            let input_u128 = byte_array_to_u128(&input_bytes);
            let key_u128 = byte_array_to_u128(&key_bytes);
            let handrolled_output_u128 = aes128(input_u128, key_u128);

            assert_eq_hex!(handrolled_output_u128, openssl_output_u128);
        }
    }
}
