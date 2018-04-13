from construct import *
from zlib import decompress, MAX_WBITS


TAG_List = Struct(
    "type_id" / Byte,
    "length" / Int,
    "data" / Switch(this.type_id, {
        0: Pass,
        1: Byte[this.length],
        2: Short[this.length],
        3: Int[this.length],
        4: Long[this.length],
        5: Float32b[this.length],
        6: Double[this.length],
        7: Struct("count" / Int, "data" / Byte[this.count])[this.length],
        8: PascalString(Short, "utf-8")[this.length],
        9: LazyBound(lambda _: TAG_List[this.length]),
        10: LazyBound(lambda _: TAG_Compound[this.length]),
        11: Struct("count" / Int, "data" / Int[this.count])[this.length],
        12: Struct("count" / Int, "data" / Long[this.count])[this.length]
        })
    )

TAG_Compound = Struct(
    "data" / RepeatUntil(lambda obj,lst,ctx: obj.type_id == 0, LazyBound(lambda _: NAMED_TAG))
    )

NAMED_TAG = Struct(
    "type_id" / Byte,
    "name" / If(this.type_id != 0, PascalString(Short, "utf-8")),
    "value" / If(this.type_id != 0, Switch(this.type_id, {
        0: Pass,
        1: Byte,
        2: Short,
        3: Int,
        4: Long,
        5: Float32b,
        6: Double,
        7: Struct("count" / Int, "data" / Byte[this.count]),
        8: PascalString(Short, "utf-8"),
        9: LazyBound(lambda _: TAG_List),
        10: LazyBound(lambda _: TAG_Compound),
        11: Struct("count" / Int, "data" / Int[this.count]),
        12: Struct("count" / Int, "data" / Long[this.count])
        }))
    )
NBT = NAMED_TAG
'''
with open("bigtest.nbt", "rb") as io:
    data = decompress(io.read(), MAX_WBITS | 16)
    c = NAMED_TAG.parse(data)
    print(c)
'''