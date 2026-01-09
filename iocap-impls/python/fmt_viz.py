from libfmtviz.bits import Bits, python_str_rows, split_bits_into_rows
from libfmtviz.render_html import divify, divify_rows

# A caveat that encodes up to 16 subdivisions and permissions masking.
# Inactive if perms == 0 or (log_d_minus_1 = 0 and i = 0xFFFF)?
CAVEAT_4_16_2PERM_FORMAT = [
    Bits("log_d_minus_1", 4), # denominator d = 2**(1+log_d_minus_1), range = [2, 2**16]
    Bits("i", 16),    # numerator i = i, range = [0, 2**16-1]
    Bits("r", 1),
    Bits("w", 1),
]

# A caveat that encodes up to 16 subdivisions without permissions masking.
# Inactive if (log_d_minus_1 = 0 and i = 0xFFFF)?
CAVEAT_4_16_FORMAT = [
    Bits("log_d_minus_1", 4), # denominator d = 2**(1+log_d_minus_1)
    Bits("i", 16),    # numerator i = i
]

# A caveat that encodes up to 15 subdivisions and permissions masking.
# Inactive if perms == 0 or log_d = 0 i.e. d = 1 => no dividing.
CAVEAT_4_15_2PERM_FORMAT = [
    Bits("log_d", 4), # denominator d = 2**(1+log_d), range = [1, 2**15]
    Bits("i", 15),    # numerator i = i, range = [0, 2**15-1]
    Bits("r", 1),
    Bits("w", 1),
]

# A caveat that encodes up to 15 subdivisions and linear permissions masking (r/w -> r, no write-only).
# Inactive if log_d = 0 i.e. d = 1 => no dividing.
CAVEAT_4_15_1PERM_FORMAT = [
    Bits("log_d", 4), # denominator d = 2**(1+log_d), range = [1, 2**15]
    Bits("i", 15),    # numerator i = i, range = [0, 2**15-1]
    Bits("r", 1), # rw or r
]

# A caveat that encodes up to 15 subdivisions without permissions masking.
# Inactive if log_d = 0 i.e. d = 1 => no dividing.
CAVEAT_4_15_FORMAT = [
    Bits("log_d", 4), # denominator d = 2**(1+log_d), range = [1, 2**15]
    Bits("i", 15),    # numerator i = i, range = [0, 2**15-1]
]

ORIGINAL_FORMAT = [
    Bits("signature", 128),

    Bits("lg2len", 8),
    Bits("base", 56),

    Bits("?", 4),
    Bits("c3", 20),
    Bits("c2", 20),
    Bits("c1", 20),
]

# A sliding window format with floating-point length, and a fixed amount of "base bits" which slide between 0 and 64 based on the length exponent.
# Allows expressing 64B regions aligned to 64B up to the first eighth of the address space
SLIDING_WINDOW_64B_CAVEATABLE_FORMAT = [
    Bits("signature", 128),

    Bits("sliding base", 55),
    Bits("l_man", 3),
    Bits("l_exp", 6),

    Bits("?", 4),
    Bits("c3", 20),
    Bits("c2", 20),
    Bits("c1", 20),
]

# A sliding window format with floating-point length, and a fixed amount of "base bits" specifying base[63:9]
# Allows expressing 512B+ regions aligned to 512B
FIXED_WINDOW_512B_2_CAVEAT_FORMAT = [
    Bits("signature", 128),

    Bits("base", (63, 9)),
    Bits("l_man", 3),
    Bits("l_exp", 6),

    Bits("key", 22),
    Bits("cav A (4.15.1perm)", 20),
    Bits("cav B (4.15.1perm)", 20),
    Bits("r", 1), # rw or r
    Bits("0", 1),
]

# A fixed-window format with floating-point length, and a fixed amount of "base bits" specifying base[63:6]
# Allows expressing 64B+ regions aligned to 64B
# Allows one 4.15.1perm caveat
FIXED_WINDOW_64B_1_CAVEAT_FORMAT =  [
    Bits("signature", 128),

    Bits("base", (63, 6)), # 58 bits
    Bits("l_exp", 6),

    Bits("key", 22),
    Bits("cav (4.15.1perm)", 20),
    Bits("l_man", 20),
    Bits("r", 1), # rw or r
    Bits("1", 1),
]

# A fixed-window format with floating-point length, and a fixed amount of "base bits" specifying base[63:6]
# Allows expressing 64B+ regions aligned to 64B
# Sacrifices the 4.15.1perm caveat for extra key space - storing 2**41 keys would require 2**45 = 32TiB of pure key storage, so it seems unlikely.
# Is this a wasteful format?
#
# For the base+length: 64 + 20 = 84 bits.
# If length mantissa were reduced to 14, it would be 64 + 14 = 78 bits.
FIXED_WINDOW_64B_KEYEXT_FORMAT =  [
    Bits("signature", 128),

    Bits("base", (63, 6)), # 58 bits
    Bits("l_exp", 6),

    Bits("key", 42),
    Bits("l_man", 20),
    Bits("r", 1), # rw or r
    Bits("1", 1),
]

# Random thought: mobile SoCs will not have >255TiB for the forseeable future, so could take advantage of this to use less space in the base addr
FIXED_WINDOW_48ADDR_64B_KEYEXT_FORMAT = [
    Bits("signature", 128),

    Bits("base", (47, 6)), # 42 bits
    Bits("l_exp", 6),
    Bits("l_man", 16),

    Bits("key", 61), # HUGE waste
    Bits("r", 1),
    Bits("w", 1),
    Bits("1", 1),
]

# From TR-951 p88
CHERI_CONCENTRATE_128 = [
    Bits("B_E", 3),
    Bits("B", (13,3)),
    Bits("T_E", 3),
    Bits("T", (11,3)),
    Bits("I_E", 1),
    Bits("otype", 18),
    Bits("?", 3),
    Bits("p", 16),

    Bits("a", 64),
]

# A revision of TR-951 p88 where the base addr always equals the addr.
# Rename a to b_top and remove 14 bits from it (a[E+13:E] and B[13:0] are redundant).
#
# Implications:
# - c_b === 0 because A3 === B3
# - if I_E = 0:
#       E = 0
#       T[13:12] = B[13:12] + (T[11:0] < B[11:0] ? 1 : 0)
#       b = b_top'50         | B[13:3] | B_E'3
#       t = (b_top'50 + c_t) | T[13:3] | T_E'3
#       t - b = (c_t << 14) + ({T[13:3], T_E} - {B[13:3], B_E}) (c_t is effectively the carry bit from T - B)
# - else:
#       E = {T_E, B_E} and implicitly 0 < E <= (64 - 14 = 50)
#       T[13:12] = B[13:12] + (T[11:3] < B[11:3] ? 1 : 0) + 1
#       b = b_top[50:E]         | B[13:3] | 0'3 | 0'E
#       t = (b_top[50:E] + c_t) | T[13:3] | 0'3 | 0'E
#
#
# SAMUEL NOTE: THIS DOESN'T WORK, BECAUSE WE LOSE INFORMATION!
# We can reconstruct T and B in full without the addr, but the actual top bits of the top and base depend on c_t and c_b which depend on the *three-way-inequality* B <= A <= T.
CHERI_CONCENTRATE_128_B_eq_A = [
    Bits("B_E", 3),
    Bits("B", (13,3)),
    Bits("b_top", 50),

    Bits("T_E", 3),
    Bits("T", (11,3)),
    Bits("I_E", 1),
    Bits("otype", 18),
    Bits("?", 3),
    Bits("p", 16),
]

# A revision of CHERI_CONCENTRATE_128_B_eq_A where min exponent is 6, not 0.
# Remove 6 bits from b_top.
# Address 2^58 64B cachelines i.e. all aligned cache lines spanning the full address space.
#
# Implications:
# - if I_E = 0:
#       E = 6
#       T[13:12] = B[13:12] + (T[11:0] < B[11:0] ? 1 : 0)
#       b = b_top'44         | B[13:3] | B_E'3 | 0'6
#       t = (b_top'44 + c_t) | T[13:3] | T_E'3 | 0'6
#       t - b = (T - B) << 6 (a 15-bit value shifted by 6)
# - else:
#       E = {T_E, B_E} and implicitly 6 < E <= (64 - 14 = 50)
#       T[13:12] = B[13:12] + (T[11:3] < B[11:3] ? 1 : 0) + 1
#       b = b_top[44:E-6]         | B[13:3] | 0'3 | 0'E
#       t = (b_top[44:E-6] + c_t) | T[13:3] | 0'3 | 0'E
#       t - b = (T - B) << E) (a 15-bit value shifted by E>6)
#
# I *think* it is more space efficient to store T and B, and implicitly get an extra bit of L from the difference between them, than it is to store B and L.
# However, I'm not sure it's better if caveats are used.
CHERI_CONCENTRATE_128_B_eq_A_64B_align = [
    Bits("b_top", 44),
    Bits("B", (13,3)),
    Bits("B_E", 3),
    Bits("T_E", 3),
    Bits("T", (11,3)), # This gets split down the 64B boundary - 3 bits in the first line, 6 in the second.
    Bits("I_E", 1),

    # total bits for 14-bit mantissa + fully addressable 64B cache lines = 71
    # This uses 7 fewer bits than a 14-bit-mantissa variant of FIXED_WINDOW_64B_KEYEXT_FORMAT, and can represent most of the same base/lengths. It sacrifices 64B alignment for sizes over 2^20, and sizes over 2^20 only get 11 bits of size-precision rather than 14.
    # A region of size 2^N (N >= 20) gets a min alignment of 2^(N - 20 + 9), and 11 bits of size-precision
    # This delta is constant no matter the mantissa size - if we changed this version to use 20-bit mantissas we'd add 6 to T, add 6 to B, and remove 6 from B_top. FIXED_WINDOW_64B_KEYEXT_FORMAT also adds 6 bits.
    # However! This may not be suitable for using with caveats, because we don't have easy access to the length. Moreover, pow2div caveats don't work with non-pow2 lengths, and this format is designed to give high-precision non-pow2 lengths at the low end.
    # This is much more appropriate for a KEYEXT format than it is for a caveat format.

    Bits("other", 57),
]

# Re-structure of CHERI_CONCENTRATE_128_B_eq_A_64B_align as a base/length style.
#
# if I_E == 0:
#   E = 6
#   base[63:0] =      B[58:3    | B_E[2:0] | 0'6 
#   len[6+13:0] = 0'1 | L[11:3] | L_E[2:0] | 0'6 (12 effective bits of length)
#   top = base + len (err on overflow)
# elif I_E == 1:
#   E = {L_E, B_E} >= 6
#   base[63:0]  =    B[58:3]    |    0'9    (is there any reason to waste bits zeroing out stuff as part of the exponent? I don't think so)
#   len[E+13:0] = 1'1 | L[11:3] | 0'3 | 0'E 
#
# In conclusion: there is advantage to encoding top instead of length. This is because you can skip encoding the top two bits of top.
# T[N:N-2] can be derived from logic:
# - top >= base => T[N:N-2] >= B[N:N-2]. Keep a local variable t = B[N:N-2].
# - because the lowest possible value of Exponent is always used, if it is nonzero it must encode the location of the top bit of length. if I_E != 0, t += 1.
# - the final value of top must be greater than the final value of base. this means if the encoded value of T[N-3:] <= encoded value of B[N-3:], t += 1;
# T[N:N-2] = B[N:N-2] + (T[N-3:] <= B[N-3:] ? 1 : 0) + I_E
CC128_STYLE_BASE_LENGTH_64B_align = [
    Bits("B_E", 3),
    Bits("B", (58, 3)),
    Bits("L_E", 3),
    Bits("L", (11, 3)), # 11-3 = 8 => 9 bits long
    Bits("I_E", 1),

    Bits("other", 57),
]

# A revision of CHERI_CONCENTRATE_128_B_eq_A where E is either 6 or 12, no inbetween?
# This is unfeasable because it limits length to 2^28. D'oh.

CC128_OUT_NAIVE = [
    Bits("address", 64),

    Bits("\(B\)", (13,0)),
    Bits("\(T\)", (13,0)),
    Bits("\(E\)", 6),
]

CC128_OUT_INTERNAL_EXP = [
    Bits("address", 64),

    Bits("\(B_E\)", 3),
    Bits("\(B\)", (13,3)),
    Bits("\(T_E\)", 3),
    Bits("\(T\)", (13,3)),
    Bits("\(I_E\)", 1),
]

CC128_OUT_INTERNALEXP_IE0 = [
    Bits("address", 64),

    # Bits("\(B\)", (2, 0)),
    Bits("\(B\)", (13,0)),
    # Bits("\(T\)", (2, 0)),
    Bits("\(T\)", (13,0)),
    Bits("\(I_E = 0\)", 1),
]

CC128_OUT_INTERNALEXP_IE1 = [
    Bits("address", 64),

    Bits("\(E\)", (2, 0)),
    Bits("\(B\)", (13,3)),
    Bits("\(E\)", (5, 3)),
    Bits("\(T\)", (13,3)),
    Bits("\(I_E = 1\)", 1),
]


CC128_OUT_T_IMPLICIT_TOP = [
    Bits("address", 64),

    Bits("\(B_E\)", 3),
    Bits("\(B\)", (13,3)),
    Bits("\(T_E\)", 3),
    Bits("\(T\)", (11,3)),
    Bits("\(I_E\)", 1),
]

CC128_OUT_FINAL = [
    Bits("address", 64),

    Bits("\(B_E\)", 3),
    Bits("\(B\)", (13,3)),
    Bits("\(T_E\)", 3),
    Bits("\(T\)", (11,3)),
    Bits("\(I_E\)", 1),

    Bits("metadata", 18+3+16),
]

FINAL_PERFECT_FORMAT = [
#     Bits("\(E\)", 5),
#     Bits("\(B\) | \(C\)", 59),
    Bits("\(B, C, E\)", 64),

    Bits("caveats", 38),
    Bits("secret ID", 22),
    Bits("meta", 4),
]

FINAL_PERFECT_FORMAT_0 = [
    Bits("\(E\)", 6),
    Bits("\(B\) | \(C\)", 69),

    Bits("?", 1),
    Bits("secret ID", 48),
    Bits("0 cav", 4),
]

FINAL_PERFECT_FORMAT_1 = [
    Bits("\(E\)", 5),
    Bits("\(B\) | \(C\)", 59),

    Bits("\(X_1\)", 19),
    Bits("\(Y_1\)", 19),
    Bits("secret ID", 22),
    Bits("1 cav", 4),
]

FINAL_PERFECT_FORMAT_2 = [
    Bits("\(E\)", 5),
    Bits("\(B\) | \(C\)", 59),

    Bits("\(\\text{S}_{Div}\)", 4),
    Bits("\(\\text{Index}\)", 15),
    Bits("\(X_2\)", 9),
    Bits("\(Y_2\)", 9),
    Bits("?", 1),
    Bits("secret ID", 22),
    Bits("2 cav", 4),
]

CAP2024_02_FORMAT = [
    Bits("Exp", 5),
    Bits("Base | Count", 59),

    Bits("Index Caveat", 19),
    Bits("Range Caveat", 18),
    Bits("Secret Key ID", 23),
    Bits("Perms", 4),
]

if __name__ == '__main__':
    # print(divify(FINAL_PERFECT_FORMAT))
    # print(divify(FINAL_PERFECT_FORMAT_0))
    # print(divify(FINAL_PERFECT_FORMAT_1))
    # print(divify(FINAL_PERFECT_FORMAT_2))
    print(divify(CAP2024_02_FORMAT))