from construct import *

Slot = Struct(
    "blockId" / Int16,
    "itemCount" / If(this.blockid > -1, Int8),
    "itemDamage" / If(this.blockid > -1, Int16),
    "nbtData" / If(this.blockid > -1, OptioanlNbt))

Position = BitStruct(
    "x" / BitsInteger(26, signed=True),
    "y" / BitsInteger(12, signed=True),
    "z" / BitsInteger(26, signed=True))
