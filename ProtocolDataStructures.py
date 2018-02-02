from construct import *

slot = Struct(
    "blockId" / Int16,
    "itemCount" / If(this.blockid > -1, Int8),
    "itemDamage" / If(this.blockid > -1, Int16),
    "nbtData" / If(this.blockid > -1, OptioanlNbt)
    )