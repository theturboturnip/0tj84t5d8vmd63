from dataclasses import dataclass
from typing import List, Optional, Tuple, Union


@dataclass
class Bits:
    field_name: str
    field_bits: Tuple[int, int]
    """The bits of the field specified in verilog syntax: [51:0] = (51, 0)"""

    def __init__(self, field_name: str, field_bits: Union[int, Tuple[int,int]]):
        self.field_name = field_name

        if isinstance(field_bits, int):
            self.field_bits = (field_bits-1, 0)
        else:
            assert isinstance(field_bits, tuple)
            assert field_bits[0] >= field_bits[1]
            self.field_bits = field_bits

    def bitlen(self):
        return self.field_bits[0] - self.field_bits[1] + 1

    def __str__(self) -> str:
        return f"{self.field_name}[{self.field_bits[0]}:{self.field_bits[1]}]"
    

def split_bits_into_rows(bs: List[Bits], MAX_ROW_LEN: int=64) -> List[List[Bits]]:
    """
    Given a list of Bits, split them into list of rows of Bits of a given length.
    """
    rows = []
    curr_row = []
    curr_row_len = 0

    def add_b_to_curr_row(b: Bits) -> Optional[Bits]:
        nonlocal rows, curr_row, curr_row_len

        new_len = curr_row_len + b.bitlen()
        if new_len < MAX_ROW_LEN:
            # Push b onto the row
            curr_row.append(b)
            curr_row_len = new_len
            return None # Don't need to split anything
        elif new_len == MAX_ROW_LEN:
            # Push b onto the row
            curr_row.append(b)
            # Push the row into the list of rows, make a new row
            rows.append(curr_row)
            curr_row = []
            curr_row_len = 0
            return None # Don't need to split anything
        else: # new_len > MAX_ROW_LEN
            # Split b
            split_width = MAX_ROW_LEN - curr_row_len
            first_sw_bits_of_b = Bits(field_name=b.field_name, field_bits=(b.field_bits[1]+split_width-1, b.field_bits[1]))
            other_bits_of_b = Bits(field_name=b.field_name, field_bits=(b.field_bits[0], b.field_bits[1]+split_width))
            # Push first_sw_bits_of_b onto the row
            curr_row.append(first_sw_bits_of_b)
            # Push the row into the list of rows, make a new row
            rows.append(curr_row)
            curr_row = []
            curr_row_len = 0
            # In the next iteration, continue adding bits from other_bits_of_b
            return other_bits_of_b

    original_bs_iter = iter(bs)
    split_b = None

    while True:
        if split_b is not None:
            b = split_b # Try to add "split_b" to the current row
        else:
            try:
                # Pull the next "b" from the iterator and try to add it to the current row
                b = next(original_bs_iter)
            except StopIteration:
                break

        split_b = add_b_to_curr_row(b)

    if curr_row:
        rows.append(curr_row)
    return rows


def python_str_rows(rows: List[List[Bits]]):
    return "\n".join(" | ".join(str(b) for b in row) for row in rows)