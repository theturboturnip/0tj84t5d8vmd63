import abc
from dataclasses import dataclass
from typing import Generator, Optional, Sequence

from libfmtviz.bits import Bits
from libfmtviz.util import msb_of

BYTE_SUFFIXES = {
    0: "B",
    10: "KiB",
    20: "MiB",
    30: "GiB",
    40: "TiB",
    50: "PiB",
    60: "EiB",
}
@dataclass
class ByteOffset:
    offset: int

    def __str__(self) -> str:
        offset_msb = msb_of(self.offset)
        div = max(d for d in BYTE_SUFFIXES.keys() if offset_msb >= d)
        return f"{self.offset*1.0/(2 ** div)}{BYTE_SUFFIXES[div]}"
    
    def _repr_pretty_(self, p, cycle):
        p.text(str(self))

@dataclass
class RangeFormatStats:
    # The minimum and maximum length values for which these alignments apply
    length_min: int
    length_max_excl: int

    min_base_align_bits: int # The minimum alignment of the base address
    min_length_align_bits: int # The minimum alignment of range length

    def __str__(self) -> str:
        return f"RangeFormatStats(length_min={ByteOffset(self.length_min)}, length_max_excl={ByteOffset(self.length_max_excl)}, min_base_align_bits=2^{self.min_base_align_bits}, min_length_align=2^{self.min_length_align_bits})"



class RangeFormat(abc.ABC):
    """
    A description of a format that encodes an address range, either as (base, length) or (base, top).
    """

    @abc.abstractproperty
    def addrspace_width(self) -> int:
        ...

    @abc.abstractmethod
    def format_bits(self) -> Sequence[Bits]:
        ...

    @abc.abstractmethod
    def get_stats_for_length(self, range_length: int) -> Optional[RangeFormatStats]:
        ...

    @abc.abstractproperty
    def min_length(self) -> int:
        ...

    def generate_stats(self) -> Generator[RangeFormatStats, None, None]:
        len = self.min_length
        stats = self.get_stats_for_length(len)
        while stats is not None:
            yield stats
            # TODO is this sufficient sampling for dynamic formats which try to minimize exponent?
            len = len << 1
            stats = self.get_stats_for_length(len)
