from construct import *
import json

#with open("r", "protocol.json") as io:
#    data = json.loads(io.read())

packet_statistics = Struct(
    "entries" / PrefixedArray(VarInt,
            Struct(
                    "name" / PascalString(VarInt, "utf-8"),
                    "value" / VarInt
                )
        )
)

packet_boss_bar = Struct(
    "entityUUID" / String(36, "utf-8"),
    "action" / VarInt,
    "title" / Switch(this.action,
        {
            0: PascalString(VarInt, "utf-8"),
            3: PascalString(VarInt, "utf-8")
        }),
    "health" / Switch(this.action,
        {
        0: Float32b,
        2:Float32b
        }),
    "color" / Switch(this.action,
        {
        0:VarInt,
        4:VarInt
        }),
    "dividers" / Switch(this.action,
        {
        0:VarInt,
        4:VarInt
        }),
    "flags" / Switch(this.action,
        {
        0:Int8un,
        5:Int8un
        })
    )