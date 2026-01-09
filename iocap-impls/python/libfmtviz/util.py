def lsb_of(x):
    """Returns the index, counting from 0, of the
    least significant set bit in `x`.
    """
    return (x&-x).bit_length()-1

def msb_of(x):
    """Returns the index, counting from 0, of the
    most significant set bit in `x`.
    """
    return x.bit_length() - 1