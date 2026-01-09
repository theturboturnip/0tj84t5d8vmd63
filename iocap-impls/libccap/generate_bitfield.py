"""
This python program generates code in various langauges (TODO just C++ for now) to pack and unpack structures from primitive types
"""

from collections import defaultdict
import dataclasses
import math
import sys
from typing import Dict, List, Literal, Optional, Tuple, Union
import warnings

@dataclasses.dataclass(frozen=True)
class Field:
    name: str
    width: int
    # TODO allow embedding custom types convertible from primitive?

    def __post_init__(self):
        if self.width < 1:
            raise RuntimeError(f"Cannot generate field '{self.field}' with <1 size")

@dataclasses.dataclass(frozen=True)
class BackingPrimitive:
    width: int


@dataclasses.dataclass(frozen=True)
class BackingArray:
    of: BackingPrimitive
    len: int
    msb: bool

    @staticmethod
    def MSB(of: BackingPrimitive, len: int) -> "BackingArray":
        return BackingArray(of, len, msb=True)
    
    @staticmethod
    def LSB(of: BackingPrimitive, len: int) -> "BackingArray":
        return BackingArray(of, len, msb=False)


U8 = BackingPrimitive(8)
U16 = BackingPrimitive(16)
U32 = BackingPrimitive(32)
U64 = BackingPrimitive(64)


Backing = Union[BackingPrimitive, BackingArray]

# A structure is an ordered list of fields.
# Fields with empty names are used to specify padding in the backing
@dataclasses.dataclass(frozen=True)
class Struct:
    name: str
    backing: Backing
    fields: List[Field]

class Generator:
    def generate(self, struct: Struct):
        ...

@dataclasses.dataclass(frozen=True)
class OrComponent:
    extract_name: str
    extract_type: BackingPrimitive
    extract_first_bit: int
    extract_width: int
    insert_position: int

class CGenerator:
    c_version: int

    struct_defs: List[str]
    functions: List[str]

    bit_types = {
        x: f"uint{x}_t"
        for x in (8, 16, 32, 64)
    }
    literal_suffix = {
        32: "u",
        64: "ul",
    }

    def __init__(self, c_version: int = 11,):
        self.c_version = c_version
        self.struct_defs = []
        self.functions = []
        pass

    def cpp_type_for_backing(self, backing: Backing) -> str:
        if isinstance(backing, BackingPrimitive):
            if backing.width in self.bit_types:
                return self.bit_types[backing.width]
            else:
                raise RuntimeError(f"C cannot express the backing type {backing}. Must be of widths {[str(x) for x in self.bit_types.keys()]}")
        else:
            # isinstance(backing, BackingArray)
            return f"{self.cpp_type_for_backing(backing.of)}[{backing.len}]"
        
    def literal_of_backing_type(self, value: int, base: int, backing: BackingPrimitive) -> str:
        if base == 16:
            literal = f"0x{value:x}"
        elif base == 10:
            literal = f"{value:d}"
        else:
            raise RuntimeError(f"C++ cannot express literals in base {base}")
        
        # TODO check it fits

        if backing.width in self.literal_suffix:
            return literal + self.literal_suffix[backing.width]
        else:
            # Take a 64-bit literal and cast it down
            return f"({self.cpp_type_for_backing(backing)}){literal}{self.literal_suffix[64]}"
        
    def layout_backing_plan(self, name: str, backing: Backing) -> List[Tuple[str, BackingPrimitive]]:
        if isinstance(backing, BackingPrimitive):
            return [(name, backing)]
        else:
            # isinstance(backing, BackingArray)
            if backing.msb:
                # index [0] = most significant
                # => put it last in the list
                return sum(
                    [
                        self.layout_backing_plan(f"(*{name})[{idx}]", backing.of)
                        for idx in reversed(range(backing.len))
                    ]
                    , start=[]
                )
            else:
                return sum(
                    [
                        self.layout_backing_plan(f"(*{name})[{idx}]", backing.of)
                        for idx in range(backing.len)
                    ]
                    , start=[]
                )

    def backing_prim_for_field(self, field: Field) -> BackingPrimitive:
        if field.width <= 8:
            return U8
        elif field.width <= 16:
            return U16
        elif field.width <= 32:
            return U32
        elif field.width <= 64:
            return U64
        else:
            raise RuntimeError(f"C cannot express the field {field}. Maximum size 64 bits")
        
    def cpp_type_for_field(self, field: Field) -> str:
        return self.cpp_type_for_backing(self.backing_prim_for_field(field))
    
    # OR bit-fields from multiple names together to create a single primitive
    def generate_or_expression(self, output_name: str, output_backing: BackingPrimitive, or_components: List[OrComponent]) -> str:
        or_strategies = []
        for or_component in or_components:
            # Shift the component down so extract_first_bit is at 0, unless we are extracting directly from bit 0
            if (or_component.extract_first_bit == 0) and False:
                comp = f"({or_component.extract_name})"
            else:
                comp = f"({or_component.extract_name} >> {self.literal_of_backing_type(or_component.extract_first_bit, base=10, backing=U32)})"
            # Mask out the N bits we want.
            # If we're going to cast the value *down* to a type with exactly the width we expect, that has the same effect as the mask and we don't need it.
            # If we're going to cast the value *up*, and the current width is exactly the width we expect, we don't need to mask.
            if False and (or_component.extract_type.width != output_backing.width) and (or_component.extract_width == min(or_component.extract_type.width, output_backing.width)):
                comp = comp
            else:
                mask = (1 << or_component.extract_width) - 1
                comp = f"({comp} & {self.literal_of_backing_type(mask, base=16, backing=or_component.extract_type)})"
            # Cast the value to the expected type
            if (or_component.extract_type.width != output_backing.width):
                comp = f"({self.cpp_type_for_backing(output_backing)}){comp}"
            # Shift the outcome back up to insert_position, unless that position is 0 and this is the only component
            if len(or_components) > 1 or or_component.insert_position > 0:
                comp = f"({comp} << {or_component.insert_position})"
            or_strategies.append(comp)

        return f"\t{output_name} = (\n\t\t" + " | \n\t\t".join(or_strategies) + "\n\t);\n"

    def argument_pointer_to_backing(self, backing: Backing, argument_name: str, const: bool) -> str:
        arg = ""
        if const:
            arg = "const "
        
        if isinstance(backing, BackingPrimitive):
            return arg + self.cpp_type_for_backing(backing) + f" *{argument_name}"
        else:
            # isinstance(backing, BackingArray)
            # e.g. int (*name)[N]
            if isinstance(backing.of, BackingArray):
                raise RuntimeError("Can't generate a pointer-to-array-of-arrays as an argument right now, out of laziness")
            return arg + self.cpp_type_for_backing(backing.of) + f" (*{argument_name})[{backing.len}]"

    def add_struct(self, struct: Struct):
        # First, generate the struct definition
        struct_code = f"struct {struct.name} {{\n\t" \
            + "\n\t".join([
                f"/** {field.width}-bit field */\n\t{self.cpp_type_for_field(field)} {field.name};"
                for field in struct.fields
                if field.name
            ]) + "\n};\n"
        self.struct_defs.append(struct_code)

        backing_c_name = "backing"
        backing_c_type = self.cpp_type_for_backing(struct.backing)
        backing_c_plan = self.layout_backing_plan(backing_c_name, struct.backing)

        # Then, generate an unpack function.
        # Make this function static because we're declaring it and defining it at the same time.
        unpack_func = f"static struct {struct.name} {struct.name}_unpack({self.argument_pointer_to_backing(struct.backing, backing_c_name, const=True)}) {{\n" \
            + f"\tstruct {struct.name} value = {{0}};\n"

        pack_plan: Dict[Tuple[str, BackingPrimitive], List[OrComponent]] = defaultdict(list)
        idx_backing_elem = 0
        idx_backing_elem_bit = 0
        for field in struct.fields:
            bits_needed = field.width
            or_components = []
            while bits_needed > 0:
                (backing_elem_name, backing_elem_prim) = backing_c_plan[idx_backing_elem]
                if (bits_needed + idx_backing_elem_bit) < backing_elem_prim.width:
                    # We don't need all of this field.
                    # Consume only the bits we need.
                    or_components.append(OrComponent(
                        backing_elem_name, backing_elem_prim,
                        extract_first_bit=idx_backing_elem_bit,
                        extract_width=bits_needed,
                        insert_position=(field.width - bits_needed)
                    ))
                    # This backing element will be packed with the requisite parts of this field
                    if field.name:
                        pack_plan[(backing_elem_name, backing_elem_prim)].append(OrComponent(
                            field.name,
                            self.backing_prim_for_field(field),
                            extract_first_bit=(field.width - bits_needed),
                            extract_width=bits_needed,
                            insert_position=idx_backing_elem_bit,
                        ))
                    idx_backing_elem_bit += bits_needed
                    bits_needed = 0
                else:
                    # (bits_needed + idx_backing_elem_bit) >= backing_elem_prim.width
                    # We need the rest of this field.
                    or_components.append(OrComponent(
                        backing_elem_name, backing_elem_prim,
                        extract_first_bit=idx_backing_elem_bit,
                        extract_width=(backing_elem_prim.width - idx_backing_elem_bit),
                        insert_position=(field.width - bits_needed)
                    ))
                    # This backing element will be packed with the requisite parts of this field
                    if field.name:
                        pack_plan[(backing_elem_name, backing_elem_prim)].append(OrComponent(
                            field.name,
                            self.backing_prim_for_field(field),
                            extract_first_bit=(field.width - bits_needed),
                            extract_width=(backing_elem_prim.width - idx_backing_elem_bit),
                            insert_position=idx_backing_elem_bit,
                        ))
                    bits_needed -= (backing_elem_prim.width - idx_backing_elem_bit)
                    idx_backing_elem += 1
                    idx_backing_elem_bit = 0
                pass
            if field.name:
                unpack_func += self.generate_or_expression(
                    f"value.{field.name}",
                    self.backing_prim_for_field(field),
                    or_components
                )
        pass

        unpack_func += "\treturn value;\n}"
        self.functions.append(unpack_func)

        # Then, generate a pack function.
        # Pack into an argument instead of returning anything, because it avoids pain points with returning arrays (can't do that in C)
        # Make this function static because we're declaring it and defining it at the same time.
        pack_func = f"static void {struct.name}_pack(const struct {struct.name}* value, {self.argument_pointer_to_backing(struct.backing, backing_c_name, const=False)}) {{\n"
        for ((backing_elem_name, backing_elem_prim), or_components) in pack_plan.items():
            pack_func += self.generate_or_expression(
                backing_elem_name, # already has {backing_c_name} at the front
                backing_elem_prim,
                [
                    OrComponent(f"value->{comp.extract_name}", comp.extract_type, comp.extract_first_bit, comp.extract_width, comp.insert_position) 
                    for comp in or_components
                ]
            )

        pack_func += f"\n}}"
        self.functions.append(pack_func)
    

    def generate_header(self, out, include_guard: str="", no_std_headers_guard: str = ""):
        if include_guard:
            print(f"#ifndef {include_guard}", file=out)
            print(f"#define {include_guard}", file=out)

        print("// Autogenerated by generate_bitfield.py", file=out)
        print("", file=out)
        
        if no_std_headers_guard:
            print(f"#if !defined({no_std_headers_guard})", file=out)
        print("#include <stdint.h>", file=out)
        if no_std_headers_guard:
            print("#endif", file=out)
        print("", file=out)

        for struct in self.struct_defs:
            print(struct, file=out)
        for function in self.functions:
            print(function, file=out)

        print("", file=out)

        if include_guard:
            print(f"#endif // {include_guard}", file=out)

def revlist(l):
    return list(reversed(l))

gen = CGenerator(c_version=11)
gen.add_struct(Struct(
    "Cap2024_11_Bits",
    BackingArray(U8, 16, msb=False),
    [
    # encoded_elem_width: 0:4,
    # b_c: 5:53,
    # index_size_div: 54:57,
    # index: 58:72,
    # range_x: 73:86,
    # // range_y_minus_one = "the index of the final block we use",
    # // calculating the actual length requires range_y to represent "the index of the first block we don't"
    # // (so the start address of that block represents the end address of the final valid block)
    # // TODO consider whether this should be named "range_y_minus_one" or if the caveat code should be reasoning about "range_y_plus_one" to find the end of the range.
    # range_y_minus_one: 87:100,
    # secret_key_id: 101:123, // 23 bits
    # perms: 124:127
        Field("encoded_elem_width", 4-0+1),
        Field("b_c", 53-5+1),
        Field("index_size_div", 57-54+1),
        Field("index", 72-58+1),
        Field("range_x", 86-73+1),
        Field("range_y_minus_one", 100-87+1),
        Field("secret_key_id", 123-101+1),
        Field("perms", 127-124+1),
    ]
))

gen.generate_header(out=sys.stdout, include_guard="LIBCCAP_BITFIELDS_INCL_C", no_std_headers_guard="LIBCCAP_NO_STANDARD_HEADERS")
