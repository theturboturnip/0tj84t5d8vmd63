"""
This module contains a sketch of an implementation of a cap2024_02 decoder.
The decoder contains 
- a frontend pipeline stage for decomposing the initial capability text
- a backend pipeline stage which intends to loop on itself for between 3 and 8 cycles to compute the output (base,top) range for the capability.

I am designing the backend pipeline stage to reuse hardware and reduce area as much as possible in terms of computational elements,
but I haven't yet addressed the problem of register allocation - this setup may require using more registers than others and as such may not be area-optimal.

The reason I prioritize computational area is because this relies heavily on 65-bit left shifts and 65-bit additions.
These are large elements and I don't want to duplicate them.
Additionally, the encryption required by these elements implies I need 10s of cycles of latency to do the signature checking, so latency up to 10 cycles is permissible regardless.

The total list of processing elements required in a single cycle is
- 65-bit shifter
- 65-bit adder/subtractor
- 16-bit adder/subtractor
- 9-bit adder/subtractor

As it stands, when using a unified pipeline for the 0, 1, 2 caveat cases (n_cav_together) each cycle aside from the frontend stage and the final cycle of the backend stage (which always computes the top = base + length) use the left shifter.
The `length` used by the final cycle is always the last thing to be computed before the final cycle, implying the cycle latencies are optimal without putting the shifter and adder together in a long path.

The shifter is intended to register inputs at the end of a cycle and immediately start calculating the output at the start of the next cycle.
The large adder is also intended to operate like this.
The smaller adders, being smaller, do not have these restrictions.
I do have some control logic for cycle N+1 depend on combinatorial functions of the outputs of the small adders in cycle N, implying a long path between the outputs of the small adders and the registers.

The total cycle count depends on the number of caveats:
- 0 caveats = 3 cycles
- 1 caveat = 6 cycles
- 2 caveats = 8 cycles
"""

from dataclasses import dataclass
from enum import Enum
from typing import Generator, Optional, Tuple, Union

class CapException(RuntimeError):
    def __init__(self, *args: object) -> None:
        super().__init__(str(type(self).__name__), *args)
    pass
class InvalidCapPermsChain(CapException):
    pass
class InvalidCaveat(CapException):
    pass
class InvalidSignature(CapException):
    pass
class UnexpectedCaveat(CapException):
    pass

class TestFail(RuntimeError):
    pass

def bits(x: int, stop: int, start: int) -> int:
    masklen = stop - start + 1
    mask = ~(-1 << masklen)
    return (x >> start) & mask

def decodeQuadrant(encoded_elem_width):
    q = bits(encoded_elem_width, 4, 3)

    if bits(encoded_elem_width, 2, 0) == 0b111:
        return (q + 1) & 0b11
    else:
        return q

def decodeImplicit1(encoded_elem_width):
    return 0 if bits(encoded_elem_width, 2, 0) == 0b111 else 1

def decodeBaseAddr(quadrant, implicit_1, b_c):
    if quadrant == 0:
        base = bits(b_c, 58, 5) << 10
        elem_count_minus_one = (implicit_1 << 5) | bits(b_c, 4, 0)
    elif quadrant == 1:
        base = bits(b_c, 58, 12) << 17
        elem_count_minus_one = (implicit_1 << 12) | bits(b_c, 11, 0)
    elif quadrant == 2:
        base = bits(b_c, 58, 19) << 24
        elem_count_minus_one = (implicit_1 << 19) | bits(b_c, 18, 0)
    elif quadrant == 3:
        base = bits(b_c, 58, 26) << 31
        elem_count_minus_one = (implicit_1 << 26) | bits(b_c, 25, 0)
    return base, elem_count_minus_one

def decodeElemWidthLog2(encoded_elem_width):
    q = bits(encoded_elem_width, 4, 3)
    if q == 0:
        return encoded_elem_width + 10
    elif q == 1:
        return encoded_elem_width + 10 - 1
    elif q == 2:
        return encoded_elem_width + 10 - 2
    elif q == 3:
        if encoded_elem_width == 31:
            return 10
        else:
            return encoded_elem_width + 10 - 3

def decodeLogMaxCount(quadrant, implicit_1):
    if quadrant == 0:
        return 5 + implicit_1
    elif quadrant == 1:
        return 12 + implicit_1
    elif quadrant == 2:
        return 19 + implicit_1
    elif quadrant == 3:
        return 26 + implicit_1

# (55 bits) |       0'13       | elem_count'15 |        0'13      | range_x'14 |
# with shift windows
# (55 bits) |  shift_elem_count'14  |   X'14   | shift_range_x'13 |    X'14    |
# which means
# shift_elem_count[13:0] = 0'13 | elem_count[14] if left shift amt = 0
# shift_elem_count[13:0] = elem_count[14:1]      if left shift amt = 13 (max)

def pack_shift_pair(range_x: int, elem_count: int) -> int:
    return bits(range_x, 13, 0) | (bits(elem_count, 14, 0) << 27)

def unpack_shift_pair(x: int) -> Tuple[int,int]:
    return bits(x, 26, 14), bits(x, 54, 41)

def zero_cav(
    # UInt#(9) range_y_minus_one;
    # UInt#(9) range_x;
    range_y_minus_one,
    range_x,

    # UInt#(15) index;
    # UInt#(4) index_size_div;
    index,
    index_size_div,

    # Bit#(59) b_c;
    # Bit#(5) encoded_elem_width;

    encoded_elem_width,
    b_c
):
    quadrant = decodeQuadrant(encoded_elem_width) # 2-bit logic
    implicit_1 = decodeImplicit1(encoded_elem_width) # 3-bit logic
    base, elem_count_minus_one = decodeBaseAddr(quadrant, implicit_1, b_c) # bit splitting
    elem_width_log_2 = decodeElemWidthLog2(encoded_elem_width) # 6-bit add
    elem_count = elem_count_minus_one + 1 # 5-bit add
    if index != 0 or index_size_div != 0 or range_x != 0 or range_y_minus_one != 0:
        raise UnexpectedCaveat()

    #-------- frontend

    length = elem_count << elem_width_log_2 # 65-bit shift

    #--------

    top = base + length # 64-bit add
    return base, top
    
def one_cav(
    # UInt#(9) range_y_minus_one;
    # UInt#(9) range_x;
    range_y_minus_one,
    range_x,

    # UInt#(15) index;
    # UInt#(4) index_size_div;
    index,
    index_size_div,

    # Bit#(59) b_c;
    # Bit#(5) encoded_elem_width;

    encoded_elem_width,
    b_c
):
    quadrant = decodeQuadrant(encoded_elem_width) # 2-bit logic
    implicit_1 = decodeImplicit1(encoded_elem_width) # 3-bit logic
    base, elem_count_minus_one = decodeBaseAddr(quadrant, implicit_1, b_c) # bit splitting
    log_max_count = decodeLogMaxCount(quadrant, implicit_1) # 5-bit add
    elem_width_log_2 = decodeElemWidthLog2(encoded_elem_width) # 6-bit add
    elem_count = elem_count_minus_one + 1 # 5-bit add
    if range_x != 0 or range_y_minus_one != 0:
        raise UnexpectedCaveat()

    initial_base = base
    initial_length = elem_count << elem_width_log_2

    #-------- frontend

    check_rhs = elem_count << index_size_div # 43-bit shift
    index_plus_one = index + 1 # 16-bit add
    elem_width_log_2 = elem_width_log_2 + log_max_count # 6-bit add

    #-------- (1)

    check_lhs = index_plus_one << log_max_count # 43-bit shift
    elem_width_log_2 = elem_width_log_2 - index_size_div # 5-bit sub
    elem_count = 1

    #-------- (2)

    check_cmp = check_rhs - check_lhs # 43-bit sub
    start_offset = index << elem_width_log_2 # 64-bit shift

    #-------- (3)

    if check_cmp < 0:
        if index == 0:
            base = base
            length = initial_length # 65-bit shift(?)
        else:
            raise InvalidCaveat()
    else:
        base = base + start_offset # 64-bit add
        length = elem_count << elem_width_log_2 # 65-bit shift

    #--------

    if base >= (1 << 64):
        raise InvalidCaveat()

    top = base + length # 64-bit add
    return base, top

def two_cav(
    # UInt#(9) range_y_minus_one;
    # UInt#(9) range_x;
    range_y_minus_one,
    range_x,

    # UInt#(15) index;
    # UInt#(4) index_size_div;
    index,
    index_size_div,

    # Bit#(59) b_c;
    # Bit#(5) encoded_elem_width;

    encoded_elem_width,
    b_c
):
    quadrant = decodeQuadrant(encoded_elem_width) # 2-bit logic
    implicit_1 = decodeImplicit1(encoded_elem_width) # 3-bit logic
    base, elem_count_minus_one = decodeBaseAddr(quadrant, implicit_1, b_c) # bit splitting
    log_max_count = decodeLogMaxCount(quadrant, implicit_1) # 5-bit add
    elem_width_log_2 = decodeElemWidthLog2(encoded_elem_width) # 6-bit add
    elem_count = elem_count_minus_one + 1 # 5-bit add

    initial_elem_count = elem_count
    initial_elem_width_log_2 = elem_width_log_2

    #-------- frontend

    check_rhs = elem_count << index_size_div # 43-bit shift
    index_plus_one = index + 1 # 16-bit add
    elem_width_log_2 = elem_width_log_2 + log_max_count # 6-bit add

    #--------

    check_lhs = index_plus_one << log_max_count # 43-bit shift
    elem_width_log_2 = elem_width_log_2 - index_size_div # 5-bit sub
    elem_count = range_y_minus_one - range_x # 9-bit sub
    
    #--------

    # Fun - this does time travel and checks caveat two before caveat one!
    if elem_count < 0:
        raise InvalidCaveat()
    start_offset = index << elem_width_log_2 # 65-bit shift
    check_cmp = check_rhs - check_lhs # 43-bit sub
    
    # TODO I am making the executive decision to allow conditionals post-small-add.
    # This may screw up something.
    elem_width_log_2_sub_14 = elem_width_log_2 - 14 # 7-bit sub

    ## AAAAAH FUCK.
    # I really really want to compute top directly here using an end_offset - shift (range_y_minus_one + 1) instead of (range_y_minus_one + 1 - range_x)
    # but I CANT because then it truncates differently in the small case!
    # Saving grace: in the small case we can guarantee the sizes are <16bit => we can use smaller things to do the add.
    # In the large case we can compute top directly because there's no truncation.
    if elem_width_log_2_sub_14 < 0:
        shift_in = pack_shift_pair(range_x, range_y_minus_one + 1 - range_x) 
        shift_amt = elem_width_log_2
        elem_width_log_2 = 0 # x-bit assign
    else:
        shift_in = range_y_minus_one + 1 # 15-bit asign
        shift_amt = elem_width_log_2_sub_14
        elem_width_log_2 = elem_width_log_2_sub_14 # 7-bit assign

    #--------

    identity_cav1 = False
    if check_cmp < 0:
        if index == 0:
            identity_cav1 = True
        else:
            raise InvalidCaveat()

    # Increment using start_offset from cav1
    base = base + start_offset # 64-bit add
    if base >= (1 << 64):
        raise InvalidCaveat()
    
    # Compute end_offset from cav2
    shift_out = shift_in << shift_amt # 65-bit shift of range_x
    if elem_width_log_2_sub_14 < 0:
        start_offset, length = unpack_shift_pair(shift_out)
    else:
        end_offset = shift_out

        # Compute start_offset
        shift_in = range_x # 14-bit assign

    #--------

    if elem_width_log_2_sub_14 < 0:
        # Increment using start_offset from cav2
        base = base + start_offset # 64-bit(?)
        if base >= (1 << 64):
            raise InvalidCaveat()
        
        end_offset = length + start_offset # 16-bit(?)
    else:
        top = base + end_offset # 64-bit

        # Find cav2 start_offset
        shift_out = shift_in << shift_amt # 65-bit shift of range_x
        start_offset = shift_out

    shift_in = initial_elem_count
    shift_amt = initial_elem_width_log_2

    #--------

    if elem_width_log_2_sub_14 < 0:
        top = base + length # 64-bit
    else:
        # Increment using start_offset from cav2
        base = base + start_offset # 64-bit
        if base >= (1 << 64):
            raise InvalidCaveat()
        
    initial_length = shift_out = shift_in << shift_amt

    #--------

    if end_offset > initial_length: # 64-bit cmp
        raise InvalidCaveat()

    return base, top
    
def n_cav(
    n_caveats,

    range_y_minus_one,
    range_x,

    index,
    index_size_div,

    encoded_elem_width,
    b_c
):
    if n_caveats == 0:
        return zero_cav(range_y_minus_one, range_x, index, index_size_div, encoded_elem_width, b_c)
    elif n_caveats == 1:
        return one_cav(range_y_minus_one, range_x, index, index_size_div, encoded_elem_width, b_c)
    elif n_caveats == 2:
        return two_cav(range_y_minus_one, range_x, index, index_size_div, encoded_elem_width, b_c)

def n_cav_together(
    n_caveats,


    range_y_minus_one,
    range_x,

    index,
    index_size_div,

    encoded_elem_width,
    b_c
):
    quadrant = decodeQuadrant(encoded_elem_width) # 2-bit logic
    implicit_1 = decodeImplicit1(encoded_elem_width) # 3-bit logic
    base, elem_count_minus_one = decodeBaseAddr(quadrant, implicit_1, b_c) # bit splitting
    log_max_count = decodeLogMaxCount(quadrant, implicit_1) # 5-bit add
    elem_width_log_2 = decodeElemWidthLog2(encoded_elem_width) # 6-bit add
    elem_count = elem_count_minus_one + 1 # 5-bit add
    if n_caveats <= 1 and (range_x != 0 or range_y_minus_one != 0): # 18-bit zero comparison (4 ORs depth + 1 AND)
        raise UnexpectedCaveat()
    if n_caveats == 0 and (index_size_div != 0 or index != 0): # 19-bit zero comparison (4 ORs depth + 1 AND)
        raise UnexpectedCaveat()
    
    initial_elem_count = elem_count
    initial_elem_width_log_2 = elem_width_log_2

    #-------------------------------------------------- frontend

    if n_caveats == 0:
        length = elem_count << elem_width_log_2 # 65-bit shift

        #------------------------------------------------

        top = base + length
        return base, top
    else:
        check_rhs = elem_count << index_size_div # 43-bit shift
        index_plus_one = index + 1 # 16-bit add
        elem_width_log_2 = elem_width_log_2 + log_max_count # 6-bit add

        #------------------------------------------------

        check_lhs = index_plus_one << log_max_count # 43-bit shift
        elem_width_log_2 = elem_width_log_2 - index_size_div # 5-bit sub
        if n_caveats == 2:
            elem_count = range_y_minus_one - range_x # 9-bit sub
        else:
            elem_count = 1
        
        #--------------------------------------------------

        # Fun - this does time travel and checks caveat two before caveat one!
        if elem_count < 0:
            raise InvalidCaveat()

        check_cmp = check_rhs - check_lhs # 43-bit sub
        start_offset = index << elem_width_log_2 # 65-bit shift

        if n_caveats == 2:
            # TODO I am making the executive decision to allow conditionals post-small-add.
            # This may screw up something.
            elem_width_log_2_sub_14 = elem_width_log_2 - 14 # 7-bit sub
            elem_count = elem_count + 1 # 10-bit add

            if elem_width_log_2_sub_14 < 0:
                shift_in = pack_shift_pair(range_x, elem_count) 
                shift_amt = elem_width_log_2
                elem_width_log_2 = 0 # x-bit assign
            else:
                shift_in = range_y_minus_one + 1 # 14-bit add
                shift_amt = elem_width_log_2_sub_14
                elem_width_log_2 = elem_width_log_2_sub_14 # 7-bit assign

        #---------------------------------------------------

        identity_cav1 = False
        if check_cmp < 0:
            if index == 0:
                identity_cav1 = True
            else:
                raise InvalidCaveat()

        # Increment using start_offset from cav1
        base = base + start_offset # 64-bit add
        if base >= (1 << 64):
            raise InvalidCaveat()
    
        if n_caveats == 1:
            if identity_cav1:
                length = initial_elem_count << initial_elem_width_log_2 # 65-bit shift
            else:
                length = 1 << elem_width_log_2 # 65-bit shift

            #--------

            top = base + length # 64-bit add
            return base, top

        else:
            # Compute end_offset from cav2
            shift_out = shift_in << shift_amt # 65-bit shift of either (range_x, elem_count) or range_y_minus_one+1
            if elem_width_log_2_sub_14 < 0:
                start_offset, length = unpack_shift_pair(shift_out)
            else:
                end_offset = shift_out

                # Compute start_offset
                shift_in = range_x # 14-bit assign

            #--------

            if elem_width_log_2_sub_14 < 0:
                # Increment using start_offset from cav2
                base = base + start_offset # 64-bit(?)
                if base >= (1 << 64):
                    raise InvalidCaveat()
                
                end_offset = length + start_offset # 16-bit(?)
            else:
                top = base + end_offset # 64-bit

                # Find cav2 start_offset
                shift_out = shift_in << shift_amt # 65-bit shift of range_x
                start_offset = shift_out

            shift_in = initial_elem_count
            shift_amt = initial_elem_width_log_2

            #--------

            if elem_width_log_2_sub_14 < 0:
                top = base + length # 64-bit
            else:
                # Increment using start_offset from cav2
                base = base + start_offset # 64-bit
                if base >= (1 << 64):
                    raise InvalidCaveat()
                
            initial_length = shift_out = shift_in << shift_amt

            #--------

            if end_offset > initial_length: # 64-bit cmp
                raise InvalidCaveat()
            
            return base, top


class AddSubOp(Enum):
    ADD = 0,
    SUB = 1,

def eval_add_sub_op(op: Tuple[AddSubOp, int, int], width: int):
    mask = (1 << width) - 1
    if op[0] == AddSubOp.ADD:
        return (op[1] + op[2]) & mask
    else:
        val = (op[1] - op[2])
        # TODO if negative, treat as a `width`-width signed integer?
        if val < 0:
            return val
        else:
            return val & mask

@dataclass
class CalcState:
    shl_to_shift: int
    shl_amt: int

    add_op_65: Tuple[AddSubOp, int, int]
    add_op_16: Tuple[AddSubOp, int, int]
    add_op_9: Tuple[AddSubOp, int, int]

    def calc(self) -> "CalcResults":
        return CalcResults(
            self.shl_to_shift << self.shl_amt,
            eval_add_sub_op(self.add_op_65, width=65),
            eval_add_sub_op(self.add_op_16, width=16),
            eval_add_sub_op(self.add_op_9, width=9),
        )

@dataclass
class CalcResults:
    shifted: int
    added_65: int
    added_16: int
    added_9: int

def n_cav_together_preptime(
    n_caveats,

    range_y_minus_one,
    range_x,

    index,
    index_size_div,

    encoded_elem_width,
    b_c
):
    """This is an implementation of n_cav_together that structures the computation across pipeline stages.
    
    The plan is the clock the inputs to the adders, shifters etc.
    so that their outputs can be redirected to arbitrary registers in the next cycle.
    As such, before each stage ends we create a CalcState() object describing what ops should happen at the start of the next cycle,
    and then at the start of the next stage we .calc() it to get the CalcResults()."""

    quadrant = decodeQuadrant(encoded_elem_width) # 2-bit logic
    implicit_1 = decodeImplicit1(encoded_elem_width) # 3-bit logic
    base, elem_count_minus_one = decodeBaseAddr(quadrant, implicit_1, b_c) # bit splitting
    log_max_count = decodeLogMaxCount(quadrant, implicit_1) # 5-bit add
    elem_width_log_2 = decodeElemWidthLog2(encoded_elem_width) # 6-bit add
    elem_count = elem_count_minus_one + 1 # 5-bit add
    if n_caveats <= 1 and (range_x != 0 or range_y_minus_one != 0): # 18-bit zero comparison (4 ORs depth + 1 AND)
        raise UnexpectedCaveat()
    if n_caveats == 0 and (index_size_div != 0 or index != 0): # 19-bit zero comparison (4 ORs depth + 1 AND)
        raise UnexpectedCaveat()
    
    initial_elem_count = elem_count
    initial_elem_width_log_2 = elem_width_log_2
    identity_cav1 = False

    state = CalcState(
        shl_to_shift=elem_count,
        shl_amt=elem_width_log_2 if n_caveats == 0 else index_size_div,

        add_op_65=(AddSubOp.ADD, 0, 0), # unused
        add_op_16=(AddSubOp.ADD, index, 1),
        add_op_9=(AddSubOp.ADD, elem_width_log_2, log_max_count),
    )
    #-------------------------------------------------- frontend
    res = state.calc()

    if n_caveats == 0:
        length = res.shifted # elem_count << elem_width_log_2 # 65-bit shift

        state = CalcState(
            shl_to_shift=0,
            shl_amt=0,
            add_op_65=(AddSubOp.ADD, base, length),
            add_op_16=(AddSubOp.ADD, 0, 0), # unused
            add_op_9=(AddSubOp.ADD, 0, 0), # unused
        )
        #--------------------------------------------
        res = state.calc()

        top = res.added_65 # base + length # 64-bit add
        return base, top
    else:
        check_rhs = res.shifted # elem_count << index_size_div # 43-bit shift
        index_plus_one = res.added_16 # index + 1 # 16-bit add
        elem_width_log_2 = res.added_9 # elem_width_log_2 + log_max_count # 6-bit add

        state = CalcState(
            shl_to_shift=index_plus_one,
            shl_amt=log_max_count,
            
            add_op_65=(AddSubOp.ADD, range_y_minus_one, 1),
            add_op_16=(AddSubOp.SUB, range_y_minus_one, range_x),
            add_op_9=(AddSubOp.SUB, elem_width_log_2, index_size_div),
        )
        #------------------------------------------------
        res = state.calc()

        range_y = res.added_65 # will be 15-bit
        assert range_y < (1 << 15)
        check_lhs = res.shifted # index_plus_one << log_max_count # 43-bit shift
        elem_width_log_2 = res.added_9 # elem_width_log_2 - index_size_div # 5-bit sub
        if n_caveats == 2:
            elem_count = res.added_16 # range_y_minus_one - range_x # 9-bit sub
            # Fun - this does time travel and checks caveat two before caveat one!
            if elem_count < 0:
                raise InvalidCaveat()
        else:
            elem_count = 1
        
        state = CalcState(
            shl_to_shift=index,
            shl_amt=elem_width_log_2,
            add_op_65=(AddSubOp.SUB, check_rhs, check_lhs),
            add_op_16=(AddSubOp.ADD, elem_count, 1),
            add_op_9=(AddSubOp.SUB, elem_width_log_2, 14),
        )
        #--------------------------------------------------
        res = state.calc()

        start_offset = res.shifted # index << elem_width_log_2 # 65-bit shift
        check_cmp = res.added_65 # check_rhs - check_lhs # 43-bit sub
        if check_cmp < 0:
            if index == 0:
                identity_cav1 = True
            else:
                raise InvalidCaveat()
        
        if n_caveats == 1:
            if identity_cav1:
                shl_to_shift = initial_elem_count
                shl_amt = initial_elem_width_log_2
            else:
                shl_to_shift = 1
                shl_amt = elem_width_log_2
        else:
            elem_count = res.added_16 # elem_count + 1 # 10-bit add
            elem_width_log_2_sub_14 = res.added_9 # elem_width_log_2 - 9 # 7-bit sub
            if elem_width_log_2_sub_14 < 0:                
                shl_to_shift = pack_shift_pair(range_x, elem_count) # 65-bit assign
                shl_amt = elem_width_log_2
                
                elem_width_log_2 = 0 # x-bit assign
            else:              
                shl_to_shift = range_y # 65-bit assign
                shl_amt = elem_width_log_2_sub_14

                elem_width_log_2 = elem_width_log_2_sub_14 # 7-bit assign

        state = CalcState(
            shl_to_shift=shl_to_shift,
            shl_amt=shl_amt,

            add_op_65=(AddSubOp.ADD, base, start_offset),
            add_op_16=(AddSubOp.ADD, 0, 0), # unused
            add_op_9=(AddSubOp.ADD, 0, 0), # unused
        )
        #---------------------------------------------------
        res = state.calc()

        base = res.added_65 # base + start_offset # 64-bit add
        if base >= (1 << 64):
            raise InvalidCaveat()
        
        if n_caveats == 1:
            length = res.shifted # elem_count << elem_width_log_2 # 65-bit shift

            state = CalcState(
                shl_to_shift=0,
                shl_amt=0,
                add_op_65=(AddSubOp.ADD, base, length),
                add_op_16=(AddSubOp.ADD, 0, 0), # unused
                add_op_9=(AddSubOp.ADD, 0, 0), # unused
            )
            #--------------------------------------------
            res = state.calc()

            top = res.added_65 # base + length # 64-bit add
            return base, top
        else:

            # Compute end_offset from cav2
            # shift_out = shift_in << shift_amt # 65-bit shift of either (range_x, elem_count) or range_y_minus_one+1
            # if elem_width_log_2_sub_14 < 0:
            #     start_offset, length = unpack_shift_pair(shift_out)
            # else:
            #     end_offset = shift_out

            #     # Compute start_offset
            #     shift_in = range_x # 14-bit assign


            # shift_out = shl_maybe_tuple(shift_in, shift_amt) # 65-bit shift of either (elem_count, range_x) or (range_x)
            if elem_width_log_2_sub_14 < 0:
                start_offset, length = unpack_shift_pair(res.shifted) # two halves

                shl_to_shift = 0 # unused :(
                shl_amt = 0 # unused :(

                add_op_65 = (AddSubOp.ADD, base, start_offset)

                # end_offset = length + start_offset
                add_op_16=(AddSubOp.ADD, start_offset, length)
            else:
                end_offset = res.shifted # shift_out
                shl_to_shift = range_x
                shl_amt = elem_width_log_2_sub_14
                
                add_op_65 = (AddSubOp.ADD, base, end_offset)

                add_op_16=(AddSubOp.ADD, 0, 0) # unused

            state = CalcState(
                shl_to_shift=shl_to_shift,
                shl_amt=shl_amt,

                add_op_65=add_op_65,
                add_op_16=add_op_16,
                add_op_9=(AddSubOp.ADD, 0, 0), # unused
            )
            #--------
            res = state.calc()

            # if elem_width_log_2_sub_14 < 0:
            #     # Increment using start_offset from cav2
            #     base = base + start_offset # 64-bit(?)
            #     if base >= (1 << 64):
            #         raise InvalidCaveat()
                
            #     end_offset = length + start_offset # 16-bit(?)
            # else:
            #     top = base + end_offset # 64-bit

            #     # Find cav2 start_offset
            #     shift_out = shift_in << shift_amt # 65-bit shift of range_x
            #     start_offset = shift_out

            # shift_in = initial_elem_count
            # shift_amt = initial_elem_width_log_2

            if elem_width_log_2_sub_14 < 0:
                base = res.added_65
                if base >= (1 << 64):
                    raise InvalidCaveat()
                
                end_offset = res.added_16

                add_op_65 = (AddSubOp.ADD, base, length)
            else:
                top = res.added_65

                start_offset = res.shifted

                add_op_65 = (AddSubOp.ADD, base, start_offset)

            state = CalcState(
                shl_to_shift=initial_elem_count,
                shl_amt=initial_elem_width_log_2,
                add_op_65=add_op_65,
                add_op_16=(AddSubOp.ADD, 0, 0), # unused
                add_op_9=(AddSubOp.ADD, 0, 0), # unused
            )
            #--------------------------------------------
            res = state.calc()

            # if elem_width_log_2_sub_14 < 0:
            #     top = base + length # 64-bit
            # else:
            #     # Increment using start_offset from cav2
            #     base = base + start_offset # 64-bit
            #     if base >= (1 << 64):
            #         raise InvalidCaveat()
                
            # initial_length = shift_out = shift_in << shift_amt

            if elem_width_log_2_sub_14 < 0:
                top = res.added_65
            else:
                base = res.added_65

            initial_length = res.shifted

            state = CalcState(
                shl_to_shift=0,
                shl_amt=0,
                add_op_65=(AddSubOp.SUB, initial_length, end_offset),
                add_op_16=(AddSubOp.ADD, 0, 0), # unused
                add_op_9=(AddSubOp.ADD, 0, 0), # unused
            )
            #--------------------------------------------
            res = state.calc()

            if res.added_65 < 0: # 64-bit cmp
                raise InvalidCaveat()
            
            return base, top

class CapPermsChain(Enum):
    # if 0bXX_11 then it's read/write, XX = num caveats
    ReadWrite_0Cav = 0b00_11
    ReadWrite_1Cav = 0b01_11
    ReadWrite_2Cav = 0b10_11
    # 0b1111 not used

    # if 0b00_XX where XX != 11 then it's read only, no reductions, XX = num caveats
    ReadOnly_0Cav = 0b00_00
    ReadOnly_1Cav = 0b00_01
    ReadOnly_2Cav = 0b00_10

    # if 0b10_10 then shrunk to read by first and only caveat
    ShrunkToRead_1Cav = 0b10_10
    # if 0b10_0X then 2 caveats, shrunk to read by caveat #(X+1)
    ShrunkToRead_2Cav_ByCav1 = 0b10_00
    ShrunkToRead_2Cav_ByCav2 = 0b10_01

    # if 0b11_XX where XX != 11 then it's write only, no reductions, XX caveats
    WriteOnly_0Cav = 0b11_00
    WriteOnly_1Cav = 0b11_01
    WriteOnly_2Cav = 0b11_10

    # if 0b01_10 then shrunk to write by first and only caveat
    ShrunkToWrite_1Cav = 0b01_10
    # if 0b01_0X then 2 caveats, shrunk to write by caveat #(X+1)
    ShrunkToWrite_2Cav_ByCav1 = 0b01_00
    ShrunkToWrite_2Cav_ByCav2 = 0b01_01


def test_func_decoder(func, n_caveats, cap, test: "Test"):
    try:
        base, top = func(n_caveats, cap.range_y_minus_one, cap.range_x, cap.index, cap.index_size_div, cap.encoded_elem_width, cap.b_c)
        if test.decode_err is not None:
            raise TestFail(f"{func.__name__} - Expected decode error {test.decode_err} but got None\n{cap}")
        if (base, top) != (test.base, test.top):
            raise TestFail(f"{func.__name__} - ({base:x}, {top:x} != {test.base:x}, {test.top:x})\n{cap}")
    except CapException as ex:
        if test.decode_err is None or type(test.decode_err) != type(ex):
            raise TestFail(f"{func.__name__} - Expected decode error {test.decode_err} but got {ex}\n{cap}")


@dataclass
class Cap2024_11:
    encoded_elem_width: int
    b_c: int
    index_size_div: int
    index: int
    range_x: int
    range_y_minus_one: int
    secret_key_id: int
    perms: CapPermsChain

    @staticmethod
    def from_hex(hex: str) -> "Cap2024_11":
        cap_text = int(hex, base=16)
        def cap_bits(start, stop):
            return bits(cap_text, stop, start)
            # the numbers i'm used to using are *inclusive* bit Bits isn't
            return cap_text[(127-stop):(128-start)].uint

        encoded_elem_width = cap_bits(0, 4)
        b_c = cap_bits(5, 53)
        index_size_div = cap_bits(54, 57)
        index = cap_bits(58, 72)
        range_x = cap_bits(73, 86)
        range_y_minus_one = cap_bits(87, 100)
        secret_key_id = cap_bits(101, 123)
        perms = CapPermsChain(cap_bits(124, 127))

        return Cap2024_11(
            encoded_elem_width,
            b_c,
            index_size_div,
            index,
            range_x,
            range_y_minus_one,
            secret_key_id,
            perms,
        )

@dataclass
class Test:
    secret: int
    # secret_key_id: int
    sig: int
    capability_text: str
    base: int
    top: int
    perms: str
    decode_err: Optional[object]

    def run_test(self):
        # Maybe#(CapPermsChain) chain;
        # Bit#(23) secret_key_id;

        # UInt#(9) range_y_minus_one;
        # UInt#(9) range_x;

        # UInt#(15) index;
        # UInt#(4) index_size_div;

        # Bit#(59) b_c;
        # Bit#(5) encoded_elem_width;

        cap = Cap2024_11.from_hex(self.capability_text)

        n_caveats = {
            CapPermsChain.ReadWrite_0Cav: 0,
            CapPermsChain.ReadWrite_1Cav: 1,
            CapPermsChain.ReadWrite_2Cav: 2,
            CapPermsChain.ReadOnly_0Cav: 0,
            CapPermsChain.ReadOnly_1Cav: 1,
            CapPermsChain.ReadOnly_2Cav: 2,
            CapPermsChain.ShrunkToRead_1Cav: 1,
            CapPermsChain.ShrunkToRead_2Cav_ByCav1: 2,
            CapPermsChain.ShrunkToRead_2Cav_ByCav2: 2,
            CapPermsChain.WriteOnly_0Cav: 0,
            CapPermsChain.WriteOnly_1Cav: 1,
            CapPermsChain.WriteOnly_2Cav: 2,
            CapPermsChain.ShrunkToWrite_1Cav: 1,
            CapPermsChain.ShrunkToWrite_2Cav_ByCav1: 2,
            CapPermsChain.ShrunkToWrite_2Cav_ByCav2: 2,
        }[cap.perms]

        test_func_decoder(n_cav, n_caveats, cap, self)
        test_func_decoder(n_cav_together, n_caveats, cap, self)
        test_func_decoder(n_cav_together_preptime, n_caveats, cap, self)

def hex_read(x: str) -> int:
    return int(x, base=16)

def csv_tests(path: str) -> Generator[Test, None, None]:
    with open(path, "r") as f:
        for line in f.readlines()[1:]:
            line_elems = line.strip().split(":")

            if line_elems[6] == "0":
                decode_err: Optional[CapException] = None
            elif line_elems[6] == "1":
                decode_err = InvalidCapPermsChain()
            elif line_elems[6] == "2":
                decode_err = InvalidCaveat()
            elif line_elems[6] == "3":
                decode_err = InvalidSignature()
            elif line_elems[6] == "4":
                decode_err = UnexpectedCaveat()
 
            yield Test(
                secret=hex_read(line_elems[0]),
                # secret_key_id=hex_read(line_elems[1]),
                sig=hex_read(line_elems[1]),
                capability_text=line_elems[2],
                base=hex_read(line_elems[3]),
                top=hex_read(line_elems[4]),
                perms=line_elems[5],
                decode_err=decode_err
            )

import argparse

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("testfile")
    parser.add_argument("--check-unpack", type=str)

    args = parser.parse_args()

    if args.check_unpack:
        print(Cap2024_11.from_hex(args.check_unpack))
    else:
        for i, test in enumerate(csv_tests(args.testfile)):
            try:
                test.run_test()


            except RuntimeError as e:
                raise RuntimeError(f"err in test {i}") from e 