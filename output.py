packet_abilities, _ = (Struct(
    "flags" / Int8sb,
    "flyingSpeed" / Float32b,
    "walkingSpeed" / Float32b,
),
None)


packet_respawn, _ = (Struct(
    "dimension" / Int32sb,
    "difficulty" / Int8ub,
    "gamemode" / Int8ub,
    "levelType" / PascalString(VarInt, "utf-8"),
),
None)


packet_resource_pack_send, _ = (Struct(
    "url" / PascalString(VarInt, "utf-8"),
    "hash" / PascalString(VarInt, "utf-8"),
),
None)


packet_select_advancement_tab, _ = (Struct(
    "id" / Optional(
        PascalString(VarInt, "utf-8"),
    ),
),
None)


packet_spawn_entity, _ = (Struct(
    "entityId" / VarInt,
    "objectUUID" / PaddedString(16, "utf8"),
    "type" / Int8sb,
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "pitch" / Int8sb,
    "yaw" / Int8sb,
    "intField" / Int32sb,
    "velocityX" / Int16sb,
    "velocityY" / Int16sb,
    "velocityZ" / Int16sb,
),
None)


packet_open_window, _ = (Struct(
    "windowId" / Int8ub,
    "inventoryType" / PascalString(VarInt, "utf-8"),
    "windowTitle" / PascalString(VarInt, "utf-8"),
    "slotCount" / Int8ub,
    "entityId" / Switch(
        this.inventoryType,
        {
            EntityHorse: Int32sb,
        },
        default=Pass,
    ),
),
None)


packet_tile_entity_data, _ = (Struct(
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
    "action" / Int8ub,
    "nbtData" / Select(Const(b"\x00"), NBT),
),
None)


packet_entity_head_rotation, _ = (Struct(
    "entityId" / VarInt,
    "headYaw" / Int8sb,
),
None)


packet_update_health, _ = (Struct(
    "health" / Float32b,
    "food" / VarInt,
    "foodSaturation" / Float32b,
),
None)


packet_spawn_entity_painting, _ = (Struct(
    "entityId" / VarInt,
    "entityUUID" / PaddedString(16, "utf8"),
    "title" / PascalString(VarInt, "utf-8"),
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
    "direction" / Int8ub,
),
None)


packet_remove_entity_effect, _ = (Struct(
    "entityId" / VarInt,
    "effectId" / Int8sb,
),
None)


packet_kick_disconnect, _ = (Struct(
    "reason" / PascalString(VarInt, "utf-8"),
),
None)


packet_spawn_position, _ = (Struct(
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
),
None)


packet_close_window, _ = (Struct(
    "windowId" / Int8ub,
),
None)


packet_block_break_animation, _ = (Struct(
    "entityId" / VarInt,
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
    "destroyStage" / Int8sb,
),
None)


packet_set_cooldown, _ = (Struct(
    "itemID" / VarInt,
    "cooldownTicks" / VarInt,
),
None)


packet_keep_alive, _ = (Struct(
    "keepAliveId" / Int64sb,
),
None)


packet_boss_bar, _ = (Struct(
    "entityUUID" / PaddedString(16, "utf8"),
    "action" / VarInt,
    "title" / Switch(
        this.action,
        {
            0: PascalString(VarInt, "utf-8"),
            3: PascalString(VarInt, "utf-8"),
        },
        default=Pass,
    ),
    "health" / Switch(
        this.action,
        {
            0: Float32b,
            2: Float32b,
        },
        default=Pass,
    ),
    "color" / Switch(
        this.action,
        {
            0: VarInt,
            4: VarInt,
        },
        default=Pass,
    ),
    "dividers" / Switch(
        this.action,
        {
            0: VarInt,
            4: VarInt,
        },
        default=Pass,
    ),
    "flags" / Switch(
        this.action,
        {
            0: Int8ub,
            5: Int8ub,
        },
        default=Pass,
    ),
),
None)


packet_entity_velocity, _ = (Struct(
    "entityId" / VarInt,
    "velocityX" / Int16sb,
    "velocityY" / Int16sb,
    "velocityZ" / Int16sb,
),
None)


packet_sound_effect, _ = (Struct(
    "soundId" / VarInt,
    "soundCategory" / VarInt,
    "x" / Int32sb,
    "y" / Int32sb,
    "z" / Int32sb,
    "volume" / Float32b,
    "pitch" / Float32b,
),
None)


packet_entity_look, _ = (Struct(
    "entityId" / VarInt,
    "yaw" / Int8sb,
    "pitch" / Int8sb,
    "onGround" / Flag,
),
None)


packet_bed, _ = (Struct(
    "entityId" / VarInt,
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
),
None)


packet_spawn_entity_experience_orb, _ = (Struct(
    "entityId" / VarInt,
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "count" / Int16sb,
),
None)


packet_craft_progress_bar, _ = (Struct(
    "windowId" / Int8ub,
    "property" / Int16sb,
    "value" / Int16sb,
),
None)


packet_chat, _ = (Struct(
    "message" / PascalString(VarInt, "utf-8"),
    "position" / Int8sb,
),
None)


packet_teams, _ = (Struct(
    "team" / PascalString(VarInt, "utf-8"),
    "mode" / Int8sb,
    "name" / Switch(
        this.mode,
        {
            0: PascalString(VarInt, "utf-8"),
            2: PascalString(VarInt, "utf-8"),
        },
        default=Pass,
    ),
    "prefix" / Switch(
        this.mode,
        {
            0: PascalString(VarInt, "utf-8"),
            2: PascalString(VarInt, "utf-8"),
        },
        default=Pass,
    ),
    "suffix" / Switch(
        this.mode,
        {
            0: PascalString(VarInt, "utf-8"),
            2: PascalString(VarInt, "utf-8"),
        },
        default=Pass,
    ),
    "friendlyFire" / Switch(
        this.mode,
        {
            0: Int8sb,
            2: Int8sb,
        },
        default=Pass,
    ),
    "nameTagVisibility" / Switch(
        this.mode,
        {
            0: PascalString(VarInt, "utf-8"),
            2: PascalString(VarInt, "utf-8"),
        },
        default=Pass,
    ),
    "collisionRule" / Switch(
        this.mode,
        {
            0: PascalString(VarInt, "utf-8"),
            2: PascalString(VarInt, "utf-8"),
        },
        default=Pass,
    ),
    "color" / Switch(
        this.mode,
        {
            0: Int8sb,
            2: Int8sb,
        },
        default=Pass,
    ),
    "players" / Switch(
        this.mode,
        {
            0: PrefixedArray(
                VarInt,
                PascalString(VarInt, "utf-8"),
            ),
            4: PrefixedArray(
                VarInt,
                PascalString(VarInt, "utf-8"),
            ),
            3: PrefixedArray(
                VarInt,
                PascalString(VarInt, "utf-8"),
            ),
        },
        default=Pass,
    ),
),
None)


packet_collect, _ = (Struct(
    "collectedEntityId" / VarInt,
    "collectorEntityId" / VarInt,
    "pickupItemCount" / VarInt,
),
None)


packet_multi_block_change, _ = (Struct(
    "chunkX" / Int32sb,
    "chunkZ" / Int32sb,
    "records" / PrefixedArray(
        VarInt,
        Struct(
            "horizontalPos" / Int8ub,
            "y" / Int8ub,
            "blockId" / VarInt,
        ),
    ),
),
None)


packet_vehicle_move, _ = (Struct(
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "yaw" / Float32b,
    "pitch" / Float32b,
),
None)


packet_tab_complete, _ = (Struct(
    "matches" / PrefixedArray(
        VarInt,
        PascalString(VarInt, "utf-8"),
    ),
),
None)


packet_world_border, _ = (Struct(
    "action" / VarInt,
    "radius" / Switch(
        this.action,
        {
            0: Float64b,
        },
        default=Pass,
    ),
    "x" / Switch(
        this.action,
        {
            2: Float64b,
            3: Float64b,
        },
        default=Pass,
    ),
    "z" / Switch(
        this.action,
        {
            2: Float64b,
            3: Float64b,
        },
        default=Pass,
    ),
    "old_radius" / Switch(
        this.action,
        {
            1: Float64b,
            3: Float64b,
        },
        default=Pass,
    ),
    "new_radius" / Switch(
        this.action,
        {
            1: Float64b,
            3: Float64b,
        },
        default=Pass,
    ),
    "speed" / Switch(
        this.action,
        {
            1: VarInt,
            3: VarInt,
        },
        default=Pass,
    ),
    "portalBoundary" / Switch(
        this.action,
        {
            3: VarInt,
        },
        default=Pass,
    ),
    "warning_time" / Switch(
        this.action,
        {
            4: VarInt,
            3: VarInt,
        },
        default=Pass,
    ),
    "warning_blocks" / Switch(
        this.action,
        {
            5: VarInt,
            3: VarInt,
        },
        default=Pass,
    ),
),
None)


packet_unload_chunk, _ = (Struct(
    "chunkX" / Int32sb,
    "chunkZ" / Int32sb,
),
None)


packet_set_slot, _ = (Struct(
    "windowId" / Int8sb,
    "slot" / Int16sb,
    "item" / Struct(
        "blockId" / Int16sb,
        "anon" / Switch(
            this.blockId,
            {
                -1: Pass,
            },
            default=Struct(
                "itemCount" / Int8sb,
                "itemDamage" / Int16sb,
                "nbtData" / Select(Const(b"\x00"), NBT),
            ),
        ),
    ),
),
None)


packet_world_event, _ = (Struct(
    "effectId" / Int32sb,
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
    "data" / Int32sb,
    "global" / Flag,
),
None)


packet_playerlist_header, _ = (Struct(
    "header" / PascalString(VarInt, "utf-8"),
    "footer" / PascalString(VarInt, "utf-8"),
),
None)


packet_craft_recipe_response, _ = (Struct(
    "windowId" / Int8sb,
    "recipe" / VarInt,
),
None)


packet_entity_metadata, _ = (Struct(
    "entityId" / VarInt,
    "metadata" / Pass,  # unfinished type UNKNOWN
),
None)


packet_unlock_recipes, _ = (Struct(
    "action" / VarInt,
    "craftingBookOpen" / Flag,
    "filteringCraftable" / Flag,
    "recipes1" / PrefixedArray(
        VarInt,
        VarInt,
    ),
    "recipes2" / Switch(
        this.action,
        {
            0: PrefixedArray(
                VarInt,
                VarInt,
            ),
        },
        default=Pass,
    ),
),
None)


packet_transaction, _ = (Struct(
    "windowId" / Int8sb,
    "action" / Int16sb,
    "accepted" / Flag,
),
None)


packet_held_item_slot, _ = (Struct(
    "slot" / Int8sb,
),
None)


packet_open_sign_entity, _ = (Struct(
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
),
None)


packet_set_passengers, _ = (Struct(
    "entityId" / VarInt,
    "passengers" / PrefixedArray(
        VarInt,
        VarInt,
    ),
),
None)


packet_map, _ = (Struct(
    "itemDamage" / VarInt,
    "scale" / Int8sb,
    "trackingPosition" / Flag,
    "icons" / PrefixedArray(
        VarInt,
        Struct(
            "directionAndType" / Int8sb,
            "x" / Int8sb,
            "y" / Int8sb,
        ),
    ),
    "columns" / Int8sb,
    "rows" / Switch(
        this.columns,
        {
            0: Pass,
        },
        default=Int8sb,
    ),
    "x" / Switch(
        this.columns,
        {
            0: Pass,
        },
        default=Int8sb,
    ),
    "y" / Switch(
        this.columns,
        {
            0: Pass,
        },
        default=Int8sb,
    ),
    "data" / Switch(
        this.columns,
        {
            0: Pass,
        },
        default=PrefixedBuffer(
            VarInt,
        ),
    ),
),
None)


packet_named_sound_effect, _ = (Struct(
    "soundName" / PascalString(VarInt, "utf-8"),
    "soundCategory" / VarInt,
    "x" / Int32sb,
    "y" / Int32sb,
    "z" / Int32sb,
    "volume" / Float32b,
    "pitch" / Float32b,
),
None)


packet_game_state_change, _ = (Struct(
    "reason" / Int8ub,
    "gameMode" / Float32b,
),
None)


packet_attach_entity, _ = (Struct(
    "entityId" / Int32sb,
    "vehicleId" / Int32sb,
),
None)


packet_scoreboard_display_objective, _ = (Struct(
    "position" / Int8sb,
    "name" / PascalString(VarInt, "utf-8"),
),
None)


packet_custom_payload, _ = (Struct(
    "channel" / PascalString(VarInt, "utf-8"),
    "data" / Pass,  # unfinished type RestBuffer
),
None)


packet_world_particles, _ = (Struct(
    "particleId" / Int32sb,
    "longDistance" / Flag,
    "x" / Float32b,
    "y" / Float32b,
    "z" / Float32b,
    "offsetX" / Float32b,
    "offsetY" / Float32b,
    "offsetZ" / Float32b,
    "particleData" / Float32b,
    "particles" / Int32sb,
    "data" / Switch(
        this.particleId,
        {
            37: PrefixedArray(
                VarInt,
                VarInt,
            ),
            36: PrefixedArray(
                VarInt,
                VarInt,
            ),
            38: PrefixedArray(
                VarInt,
                VarInt,
            ),
        },
        default=Pass,
    ),
),
None)


packet_scoreboard_objective, _ = (Struct(
    "name" / PascalString(VarInt, "utf-8"),
    "action" / Int8sb,
    "displayText" / Switch(
        this.action,
        {
            0: PascalString(VarInt, "utf-8"),
            2: PascalString(VarInt, "utf-8"),
        },
        default=Pass,
    ),
    "type" / Switch(
        this.action,
        {
            0: PascalString(VarInt, "utf-8"),
            2: PascalString(VarInt, "utf-8"),
        },
        default=Pass,
    ),
),
None)


packet_player_info, _ = (Struct(
    "action" / VarInt,
    "data" / PrefixedArray(
        VarInt,
        Struct(
            "UUID" / PaddedString(16, "utf8"),
            "name" / Switch(
                this._.action,
                {
                    0: PascalString(VarInt, "utf-8"),
                },
                default=Pass,
            ),
            "properties" / Switch(
                this._.action,
                {
                    0: PrefixedArray(
                        VarInt,
                        Struct(
                            "name" / PascalString(VarInt, "utf-8"),
                            "value" / PascalString(VarInt, "utf-8"),
                            "signature" / Optional(
                                PascalString(VarInt, "utf-8"),
                            ),
                        ),
                    ),
                },
                default=Pass,
            ),
            "gamemode" / Switch(
                this._.action,
                {
                    0: VarInt,
                    1: VarInt,
                },
                default=Pass,
            ),
            "ping" / Switch(
                this._.action,
                {
                    0: VarInt,
                    2: VarInt,
                },
                default=Pass,
            ),
            "displayName" / Switch(
                this._.action,
                {
                    0: Optional(
                        PascalString(VarInt, "utf-8"),
                    ),
                    3: Optional(
                        PascalString(VarInt, "utf-8"),
                    ),
                },
                default=Pass,
            ),
        ),
    ),
),
None)


packet_entity_move_look, _ = (Struct(
    "entityId" / VarInt,
    "dX" / Int16sb,
    "dY" / Int16sb,
    "dZ" / Int16sb,
    "yaw" / Int8sb,
    "pitch" / Int8sb,
    "onGround" / Flag,
),
None)


packet_scoreboard_score, _ = (Struct(
    "itemName" / PascalString(VarInt, "utf-8"),
    "action" / Int8sb,
    "scoreName" / PascalString(VarInt, "utf-8"),
    "value" / Switch(
        this.action,
        {
            1: Pass,
        },
        default=VarInt,
    ),
),
None)


packet_entity_update_attributes, _ = (Struct(
    "entityId" / VarInt,
    "properties" / PrefixedArray(
        Int32sb,
        Struct(
            "key" / PascalString(VarInt, "utf-8"),
            "value" / Float64b,
            "modifiers" / PrefixedArray(
                VarInt,
                Struct(
                    "uuid" / PaddedString(16, "utf8"),
                    "amount" / Float64b,
                    "operation" / Int8sb,
                ),
            ),
        ),
    ),
),
None)


packet_window_items, _ = (Struct(
    "windowId" / Int8ub,
    "items" / PrefixedArray(
        Int16sb,
        Struct(
            "blockId" / Int16sb,
            "anon" / Switch(
                this.blockId,
                {
                    -1: Pass,
                },
                default=Struct(
                    "itemCount" / Int8sb,
                    "itemDamage" / Int16sb,
                    "nbtData" / Select(Const(b"\x00"), NBT),
                ),
            ),
        ),
    ),
),
None)


packet_explosion, _ = (Struct(
    "x" / Float32b,
    "y" / Float32b,
    "z" / Float32b,
    "radius" / Float32b,
    "affectedBlockOffsets" / PrefixedArray(
        Int32sb,
        Struct(
            "x" / Int8sb,
            "y" / Int8sb,
            "z" / Int8sb,
        ),
    ),
    "playerMotionX" / Float32b,
    "playerMotionY" / Float32b,
    "playerMotionZ" / Float32b,
),
None)


packet_combat_event, _ = (Struct(
    "event" / VarInt,
    "duration" / Switch(
        this.event,
        {
            1: VarInt,
        },
        default=Pass,
    ),
    "playerId" / Switch(
        this.event,
        {
            2: VarInt,
        },
        default=Pass,
    ),
    "entityId" / Switch(
        this.event,
        {
            1: Int32sb,
            2: Int32sb,
        },
        default=Pass,
    ),
    "message" / Switch(
        this.event,
        {
            2: PascalString(VarInt, "utf-8"),
        },
        default=Pass,
    ),
),
None)


packet_entity_teleport, _ = (Struct(
    "entityId" / VarInt,
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "yaw" / Int8sb,
    "pitch" / Int8sb,
    "onGround" / Flag,
),
None)


packet_statistics, _ = (Struct(
    "entries" / PrefixedArray(
        VarInt,
        Struct(
            "name" / PascalString(VarInt, "utf-8"),
            "value" / VarInt,
        ),
    ),
),
None)


packet_entity_status, _ = (Struct(
    "entityId" / Int32sb,
    "entityStatus" / Int8sb,
),
None)


packet_block_action, _ = (Struct(
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
    "byte1" / Int8ub,
    "byte2" / Int8ub,
    "blockId" / VarInt,
),
None)


packet_title, _ = (Struct(
    "action" / VarInt,
    "text" / Switch(
        this.action,
        {
            0: PascalString(VarInt, "utf-8"),
            1: PascalString(VarInt, "utf-8"),
            2: PascalString(VarInt, "utf-8"),
        },
        default=Pass,
    ),
    "fadeIn" / Switch(
        this.action,
        {
            3: Int32sb,
        },
        default=Pass,
    ),
    "stay" / Switch(
        this.action,
        {
            3: Int32sb,
        },
        default=Pass,
    ),
    "fadeOut" / Switch(
        this.action,
        {
            3: Int32sb,
        },
        default=Pass,
    ),
),
None)


packet_block_change, _ = (Struct(
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
    "type" / VarInt,
),
None)


packet_spawn_entity_weather, _ = (Struct(
    "entityId" / VarInt,
    "type" / Int8sb,
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
),
None)


packet_map_chunk, _ = (Struct(
    "x" / Int32sb,
    "z" / Int32sb,
    "groundUp" / Flag,
    "bitMap" / VarInt,
    "chunkData" / PrefixedBuffer(
        VarInt,
    ),
    "blockEntities" / PrefixedArray(
        VarInt,
        NBT,
    ),
),
None)


packet_spawn_entity_living, _ = (Struct(
    "entityId" / VarInt,
    "entityUUID" / PaddedString(16, "utf8"),
    "type" / VarInt,
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "yaw" / Int8sb,
    "pitch" / Int8sb,
    "headPitch" / Int8sb,
    "velocityX" / Int16sb,
    "velocityY" / Int16sb,
    "velocityZ" / Int16sb,
    "metadata" / Pass,  # unfinished type UNKNOWN
),
None)


packet_entity_effect, _ = (Struct(
    "entityId" / VarInt,
    "effectId" / Int8sb,
    "amplifier" / Int8sb,
    "duration" / VarInt,
    "hideParticles" / Int8sb,
),
None)


packet_login, _ = (Struct(
    "entityId" / Int32sb,
    "gameMode" / Int8ub,
    "dimension" / Int32sb,
    "difficulty" / Int8ub,
    "maxPlayers" / Int8ub,
    "levelType" / PascalString(VarInt, "utf-8"),
    "reducedDebugInfo" / Flag,
),
None)


packet_advancements, _ = (Struct(
    "reset" / Flag,
    "advancementMapping" / PrefixedArray(
        VarInt,
        Struct(
            "key" / PascalString(VarInt, "utf-8"),
            "value" / Struct(
                "parentId" / Optional(
                    PascalString(VarInt, "utf-8"),
                ),
                "displayData" / Optional(
                    Struct(
                        "title" / PascalString(VarInt, "utf-8"),
                        "description" / PascalString(VarInt, "utf-8"),
                        "icon" / Struct(
                            "blockId" / Int16sb,
                            "anon" / Switch(
                                this.blockId,
                                {
                                    -1: Pass,
                                },
                                default=Struct(
                                    "itemCount" / Int8sb,
                                    "itemDamage" / Int16sb,
                                    "nbtData" / Select(Const(b"\x00"), NBT),
                                ),
                            ),
                        ),
                        "frameType" / VarInt,
                        "flags" / BitStruct(
                            "_unused" / BitsInteger(29, signed=False),
                            "hidden" / BitsInteger(1, signed=False),
                            "show_toast" / BitsInteger(1, signed=False),
                            "has_background_texture" / BitsInteger(1, signed=False),
                        ),
                        "backgroundTexture" / Switch(
                            this.flags.has_background_texture,
                            {
                                1: PascalString(VarInt, "utf-8"),
                            },
                            default=Pass,
                        ),
                        "xCord" / Float32b,
                        "yCord" / Float32b,
                    ),
                ),
                "criteria" / PrefixedArray(
                    VarInt,
                    Struct(
                        "key" / PascalString(VarInt, "utf-8"),
                        "value" / Pass,
                    ),
                ),
                "requirements" / PrefixedArray(
                    VarInt,
                    PrefixedArray(
                        VarInt,
                        PascalString(VarInt, "utf-8"),
                    ),
                ),
            ),
        ),
    ),
    "identifiers" / PrefixedArray(
        VarInt,
        PascalString(VarInt, "utf-8"),
    ),
    "progressMapping" / PrefixedArray(
        VarInt,
        Struct(
            "key" / PascalString(VarInt, "utf-8"),
            "value" / PrefixedArray(
                VarInt,
                Struct(
                    "criterionIdentifier" / PascalString(VarInt, "utf-8"),
                    "criterionProgress" / Optional(
                        Int64ub,
                    ),
                ),
            ),
        ),
    ),
),
None)


packet_named_entity_spawn, _ = (Struct(
    "entityId" / VarInt,
    "playerUUID" / PaddedString(16, "utf8"),
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "yaw" / Int8sb,
    "pitch" / Int8sb,
    "metadata" / Pass,  # unfinished type UNKNOWN
),
None)


packet_entity, _ = (Struct(
    "entityId" / VarInt,
),
None)


packet_difficulty, _ = (Struct(
    "difficulty" / Int8ub,
),
None)


packet_update_time, _ = (Struct(
    "age" / Int64sb,
    "time" / Int64sb,
),
None)


packet_experience, _ = (Struct(
    "experienceBar" / Float32b,
    "level" / VarInt,
    "totalExperience" / VarInt,
),
None)


packet_entity_equipment, _ = (Struct(
    "entityId" / VarInt,
    "slot" / VarInt,
    "item" / Struct(
        "blockId" / Int16sb,
        "anon" / Switch(
            this.blockId,
            {
                -1: Pass,
            },
            default=Struct(
                "itemCount" / Int8sb,
                "itemDamage" / Int16sb,
                "nbtData" / Select(Const(b"\x00"), NBT),
            ),
        ),
    ),
),
None)


packet_entity_destroy, _ = (Struct(
    "entityIds" / PrefixedArray(
        VarInt,
        VarInt,
    ),
),
None)


packet_animation, _ = (Struct(
    "entityId" / VarInt,
    "animation" / Int8ub,
),
None)


packet_rel_entity_move, _ = (Struct(
    "entityId" / VarInt,
    "dX" / Int16sb,
    "dY" / Int16sb,
    "dZ" / Int16sb,
    "onGround" / Flag,
),
None)


packet_camera, _ = (Struct(
    "cameraId" / VarInt,
),
None)


packet_position, _ = (Struct(
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "yaw" / Float32b,
    "pitch" / Float32b,
    "flags" / Int8sb,
    "teleportId" / VarInt,
),
None)


