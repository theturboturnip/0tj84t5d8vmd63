from collections import Counter
from dataclasses import dataclass
from typing import List

from libfmtviz.bits import Bits, split_bits_into_rows


@dataclass
class BitsHTMLBitIdxDiv:
    bit: int

    def as_html(self):
        return f'<div class="row-spacer">{self.bit}</div>'


@dataclass
class BitsHTMLDiv:
    name: str
    bitstr: str
    bitlen: int

    def __init__(self, b: Bits, use_shortened_bitlen: bool):
        self.name = b.field_name
        if b.bitlen() > 1:
            if use_shortened_bitlen and b.field_bits[1] == 0:
                self.bitstr = f"'{b.field_bits[0]+1}"
            else:
                self.bitstr = f"[{b.field_bits[0]}:{b.field_bits[1]}]"
        else:
            self.bitstr = ""
        self.bitlen = b.bitlen()

    def as_html(self):
        # <div class="row-elem" style="flex-grow: 52">
        #     base<br>
        #     [51:0]
        # </div>
        return f'<div class="row-elem" style="flex-grow: {self.bitlen}"><div>{self.name}</div><div>{self.bitstr}</div></div>'


@dataclass
class BitsHTMLSpacerDiv:
    bitlen: int

    def as_html(self):
        return f'<div style="flex-grow: {self.bitlen}"></div>'


def divify_rows(rows: List[List[Bits]], ROW_LEN: int=64) -> str:
    curr_bit = 0
    divified = ""

    # count the number of times each name is referenced.
    name_counts = Counter([
        b.field_name
        for row in rows
        for b in row
    ])

    for row in rows:
        div_row = [BitsHTMLBitIdxDiv(curr_bit)]
        row_len = 0
        for b in row:
            div_row.append(BitsHTMLDiv(b, use_shortened_bitlen=(name_counts[b.field_name] == 1)))
            curr_bit += b.bitlen()
            row_len += b.bitlen()
            div_row.append(BitsHTMLBitIdxDiv(curr_bit))
        if row_len < ROW_LEN:
            div_row.append(BitsHTMLSpacerDiv(ROW_LEN-row_len))

        row_contents_as_html = "\n\t".join(x.as_html() for x in reversed(div_row))
        # Add the rows in reverse order
        divified = f'<div class="row-64">\n\t{row_contents_as_html}\n</div><br>\n' + divified

    return divified

def divify(bits: List[Bits], ROW_LEN: int=64) -> str:
    return divify_rows(split_bits_into_rows(bits), ROW_LEN)