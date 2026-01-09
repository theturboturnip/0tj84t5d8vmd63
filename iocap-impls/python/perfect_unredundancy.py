# A conundrum

# We want to represent a (base, length) address range as (base = Base, length = 2^Exp * Count).
# Base, Exp, and Count are bit vectors that have to fit in 64 bits i.e. Base' + Exp' + Count' = 64
# Considering fixing Exp'5, and varying Count' based on the *value* of Exp. i.e. Base' + Count' = 64-5 = 59
# Of primary concern is the alignment of Base (i.e. the number of bottom zeroed bits i.e. align(Base) = 64 - Base' = Count' + 5) for any given value of Exp, so Count' should always be minimized.
# Start by assuming some minimum value of Exp, such that Exp is in the range [MIN_EXP, MIN_EXP+31].
# Assume align(Base) = MIN_EXP => MIN_EXP = Count' + 5 => count'(MIN_EXP) = MIN_EXP - 5
# This can represent lengths between 2^MIN_EXP and 2^(MIN_EXP + Count') = 2^(MIN_EXP + MIN_EXP - 5) at minimum precision.
# If we assume the value of Exp used to encode a length is the smallest required value to encode that length, then when we move up to 2^(MIN_EXP + 1) the length should surely be strictly greater than 2^(MIN_EXP+MIN_EXP-5)... right?
#       So Count has a min value of 2^(MIN_EXP+MIN_EXP-5 - (MIN_EXP+1)) = 2^(MIN_EXP-6) instead of 0.
# Thus the maximum length for Exp = MIN_EXP + 1 = 2^(MIN_EXP + 1) * (2^(MIN_EXP-6) + 2^(Count'))... EW! that's icky and we shouldn't do it. 


###### IGNORE THIS FOR NOW
###### At the top end we need to be able to represent a 2^64-length range => the top bit of Count' needs to be set and Count * 2^Exp = 2^64 => Count = 2^(64-Exp) => count'(max(Exp)) = 64-Max(Exp)