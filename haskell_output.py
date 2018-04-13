packet_abilities = Struct(
    "flags" / Int8sn,
    "flyingSpeed" / Float32b,
    "walkingSpeed" / Float32b,
),
packet_respawn = Struct(
    "dimension" / Int32sn,
    "difficulty" / Int8un,
    "gamemode" / Int8un,
    "levelType" / PascalString(VarInt, "utf-8"),
),
packet_resource_pack_send = Struct(
    "url" / PascalString(VarInt, "utf-8"),
    "hash" / PascalString(VarInt, "utf-8"),
),
packet_select_advancement_tab = Struct(
    "id" / Optional(
        PascalString(VarInt, "utf-8"),
    ),
),
packet_spawn_entity = Struct(
    "entityId" / VarInt,
    "objectUUID" / String(32),
    "type" / Int8sn,
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "pitch" / Int8sn,
    "yaw" / Int8sn,
    "intField" / Int32sn,
    "velocityX" / Int16sn,
    "velocityY" / Int16sn,
    "velocityZ" / Int16sn,
),
packet_open_window = Struct(
    "windowId" / Int8un,
    "inventoryType" / PascalString(VarInt, "utf-8"),
    "windowTitle" / PascalString(VarInt, "utf-8"),
    "slotCount" / Int8un,
    "entityId" / Switch(this.inventoryType, {
        "EntityHorse": Int32sn,
    }),
),
packet_tile_entity_data = Struct(
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
    "action" / Int8un,
    "nbtData" / Select(Value(b"\x00"), NBT),
),
packet_entity_head_rotation = Struct(
    "entityId" / VarInt,
    "headYaw" / Int8sn,
),
packet_update_health = Struct(
    "health" / Float32b,
    "food" / VarInt,
    "foodSaturation" / Float32b,
),
packet_spawn_entity_painting = Struct(
    "entityId" / VarInt,
    "entityUUID" / String(32),
    "title" / PascalString(VarInt, "utf-8"),
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
    "direction" / Int8un,
),
packet_remove_entity_effect = Struct(
    "entityId" / VarInt,
    "effectId" / Int8sn,
),
packet_kick_disconnect = Struct(
    "reason" / PascalString(VarInt, "utf-8"),
),
packet_spawn_position = Struct(
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
),
packet_close_window = Struct(
    "windowId" / Int8un,
),
packet_block_break_animation = Struct(
    "entityId" / VarInt,
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
    "destroyStage" / Int8sn,
),
packet_set_cooldown = Struct(
    "itemID" / VarInt,
    "cooldownTicks" / VarInt,
),
packet_keep_alive = Struct(
    "keepAliveId" / Int64sn,
),
packet_boss_bar = Struct(
    "entityUUID" / String(32),
    "action" / VarInt,
    "title" / Switch(this.action, {
        "0": PascalString(VarInt, "utf-8"),
        "3": PascalString(VarInt, "utf-8"),
    }),
    "health" / Switch(this.action, {
        "0": Float32b,
        "2": Float32b,
    }),
    "color" / Switch(this.action, {
        "0": VarInt,
        "4": VarInt,
    }),
    "dividers" / Switch(this.action, {
        "0": VarInt,
        "4": VarInt,
    }),
    "flags" / Switch(this.action, {
        "0": Int8un,
        "5": Int8un,
    }),
),
packet_entity_velocity = Struct(
    "entityId" / VarInt,
    "velocityX" / Int16sn,
    "velocityY" / Int16sn,
    "velocityZ" / Int16sn,
),
packet_sound_effect = Struct(
    "soundId" / VarInt,
    "soundCategory" / VarInt,
    "x" / Int32sn,
    "y" / Int32sn,
    "z" / Int32sn,
    "volume" / Float32b,
    "pitch" / Float32b,
),
packet_entity_look = Struct(
    "entityId" / VarInt,
    "yaw" / Int8sn,
    "pitch" / Int8sn,
    "onGround" / Flag,
),
packet_bed = Struct(
    "entityId" / VarInt,
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
),
packet_spawn_entity_experience_orb = Struct(
    "entityId" / VarInt,
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "count" / Int16sn,
),
packet_craft_progress_bar = Struct(
    "windowId" / Int8un,
    "property" / Int16sn,
    "value" / Int16sn,
),
packet_chat = Struct(
    "message" / PascalString(VarInt, "utf-8"),
    "position" / Int8sn,
),
packet_teams = Struct(
    "team" / PascalString(VarInt, "utf-8"),
    "mode" / Int8sn,
    "name" / Switch(this.mode, {
        "0": PascalString(VarInt, "utf-8"),
        "2": PascalString(VarInt, "utf-8"),
    }),
    "prefix" / Switch(this.mode, {
        "0": PascalString(VarInt, "utf-8"),
        "2": PascalString(VarInt, "utf-8"),
    }),
    "suffix" / Switch(this.mode, {
        "0": PascalString(VarInt, "utf-8"),
        "2": PascalString(VarInt, "utf-8"),
    }),
    "friendlyFire" / Switch(this.mode, {
        "0": Int8sn,
        "2": Int8sn,
    }),
    "nameTagVisibility" / Switch(this.mode, {
        "0": PascalString(VarInt, "utf-8"),
        "2": PascalString(VarInt, "utf-8"),
    }),
    "collisionRule" / Switch(this.mode, {
        "0": PascalString(VarInt, "utf-8"),
        "2": PascalString(VarInt, "utf-8"),
    }),
    "color" / Switch(this.mode, {
        "0": Int8sn,
        "2": Int8sn,
    }),
    "players" / Switch(this.mode, {
        "0": Array(
            VarInt,
            PascalString(VarInt, "utf-8"),
        ),
        "4": Array(
            VarInt,
            PascalString(VarInt, "utf-8"),
        ),
        "3": Array(
            VarInt,
            PascalString(VarInt, "utf-8"),
        ),
    }),
),
packet_collect = Struct(
    "collectedEntityId" / VarInt,
    "collectorEntityId" / VarInt,
    "pickupItemCount" / VarInt,
),
packet_multi_block_change = Struct(
    "chunkX" / Int32sn,
    "chunkZ" / Int32sn,
    "records" / Array(
        VarInt,
        Struct(
            "horizontalPos" / Int8un,
            "y" / Int8un,
            "blockId" / VarInt,
        ),
    ),
),
packet_vehicle_move = Struct(
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "yaw" / Float32b,
    "pitch" / Float32b,
),
packet_tab_complete = Struct(
    "matches" / Array(
        VarInt,
        PascalString(VarInt, "utf-8"),
    ),
),
packet_world_border = Struct(
    "action" / VarInt,
    "radius" / Switch(this.action, {
        "0": Float64b,
    }),
    "x" / Switch(this.action, {
        "2": Float64b,
        "3": Float64b,
    }),
    "z" / Switch(this.action, {
        "2": Float64b,
        "3": Float64b,
    }),
    "old_radius" / Switch(this.action, {
        "1": Float64b,
        "3": Float64b,
    }),
    "new_radius" / Switch(this.action, {
        "1": Float64b,
        "3": Float64b,
    }),
    "speed" / Switch(this.action, {
        "1": VarInt,
        "3": VarInt,
    }),
    "portalBoundary" / Switch(this.action, {
        "3": VarInt,
    }),
    "warning_time" / Switch(this.action, {
        "4": VarInt,
        "3": VarInt,
    }),
    "warning_blocks" / Switch(this.action, {
        "5": VarInt,
        "3": VarInt,
    }),
),
packet_unload_chunk = Struct(
    "chunkX" / Int32sn,
    "chunkZ" / Int32sn,
),
packet_set_slot = Struct(
    "windowId" / Int8sn,
    "slot" / Int16sn,
    "item" / Struct(
        "blockId" / Int16sn,
        "anon" / Switch(this.blockId, {
            "-1": Pass,
        }),
    ),
),
packet_world_event = Struct(
    "effectId" / Int32sn,
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
    "data" / Int32sn,
    "global" / Flag,
),
packet_playerlist_header = Struct(
    "header" / PascalString(VarInt, "utf-8"),
    "footer" / PascalString(VarInt, "utf-8"),
),
packet_craft_recipe_response = Struct(
    "windowId" / Int8sn,
    "recipe" / VarInt,
),
packet_entity_metadata = Struct(
    "entityId" / VarInt,
    "metadata" / Pass,  # unfinished type UNKNOWN
),
packet_unlock_recipes = Struct(
    "action" / VarInt,
    "craftingBookOpen" / Flag,
    "filteringCraftable" / Flag,
    "recipes1" / Array(
        VarInt,
        VarInt,
    ),
    "recipes2" / Switch(this.action, {
        "0": Array(
            VarInt,
            VarInt,
        ),
    }),
),
packet_transaction = Struct(
    "windowId" / Int8sn,
    "action" / Int16sn,
    "accepted" / Flag,
),
packet_held_item_slot = Struct(
    "slot" / Int8sn,
),
packet_open_sign_entity = Struct(
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
),
packet_set_passengers = Struct(
    "entityId" / VarInt,
    "passengers" / Array(
        VarInt,
        VarInt,
    ),
),
packet_map = Struct(
    "itemDamage" / VarInt,
    "scale" / Int8sn,
    "trackingPosition" / Flag,
    "icons" / Array(
        VarInt,
        Struct(
            "directionAndType" / Int8sn,
            "x" / Int8sn,
            "y" / Int8sn,
        ),
    ),
    "columns" / Int8sn,
    "rows" / Switch(this.columns, {
        "0": Pass,
    }),
    "x" / Switch(this.columns, {
        "0": Pass,
    }),
    "y" / Switch(this.columns, {
        "0": Pass,
    }),
    "data" / Switch(this.columns, {
        "0": Pass,
    }),
),
packet_named_sound_effect = Struct(
    "soundName" / PascalString(VarInt, "utf-8"),
    "soundCategory" / VarInt,
    "x" / Int32sn,
    "y" / Int32sn,
    "z" / Int32sn,
    "volume" / Float32b,
    "pitch" / Float32b,
),
packet_game_state_change = Struct(
    "reason" / Int8un,
    "gameMode" / Float32b,
),
packet_attach_entity = Struct(
    "entityId" / Int32sn,
    "vehicleId" / Int32sn,
),
packet_scoreboard_display_objective = Struct(
    "position" / Int8sn,
    "name" / PascalString(VarInt, "utf-8"),
),
packet_custom_payload = Struct(
    "channel" / PascalString(VarInt, "utf-8"),
    "data" / Pass,  # unfinished type RestBuffer
),
packet_world_particles = Struct(
    "particleId" / Int32sn,
    "longDistance" / Flag,
    "x" / Float32b,
    "y" / Float32b,
    "z" / Float32b,
    "offsetX" / Float32b,
    "offsetY" / Float32b,
    "offsetZ" / Float32b,
    "particleData" / Float32b,
    "particles" / Int32sn,
    "data" / Switch(this.particleId, {
        "37": Array(
            VarInt,
            VarInt,
        ),
        "36": Array(
            VarInt,
            VarInt,
        ),
        "38": Array(
            VarInt,
            VarInt,
        ),
    }),
),
packet_scoreboard_objective = Struct(
    "name" / PascalString(VarInt, "utf-8"),
    "action" / Int8sn,
    "displayText" / Switch(this.action, {
        "0": PascalString(VarInt, "utf-8"),
        "2": PascalString(VarInt, "utf-8"),
    }),
    "type" / Switch(this.action, {
        "0": PascalString(VarInt, "utf-8"),
        "2": PascalString(VarInt, "utf-8"),
    }),
),
packet_player_info = Struct(
    "action" / VarInt,
    "data" / Array(
        VarInt,
        Struct(
            "UUID" / String(32),
            "name" / Switch(this._.action, {
                "0": PascalString(VarInt, "utf-8"),
            }),
            "properties" / Switch(this._.action, {
                "0": Array(
                    VarInt,
                    Struct(
                        "name" / PascalString(VarInt, "utf-8"),
                        "value" / PascalString(VarInt, "utf-8"),
                        "signature" / Optional(
                            PascalString(VarInt, "utf-8"),
                        ),
                    ),
                ),
            }),
            "gamemode" / Switch(this._.action, {
                "0": VarInt,
                "1": VarInt,
            }),
            "ping" / Switch(this._.action, {
                "0": VarInt,
                "2": VarInt,
            }),
            "displayName" / Switch(this._.action, {
                "0": Optional(
                    PascalString(VarInt, "utf-8"),
                ),
                "3": Optional(
                    PascalString(VarInt, "utf-8"),
                ),
            }),
        ),
    ),
),
packet_entity_move_look = Struct(
    "entityId" / VarInt,
    "dX" / Int16sn,
    "dY" / Int16sn,
    "dZ" / Int16sn,
    "yaw" / Int8sn,
    "pitch" / Int8sn,
    "onGround" / Flag,
),
packet_scoreboard_score = Struct(
    "itemName" / PascalString(VarInt, "utf-8"),
    "action" / Int8sn,
    "scoreName" / PascalString(VarInt, "utf-8"),
    "value" / Switch(this.action, {
        "1": Pass,
    }),
),
packet_entity_update_attributes = Struct(
    "entityId" / VarInt,
    "properties" / Array(
        Int32sn,
        Struct(
            "key" / PascalString(VarInt, "utf-8"),
            "value" / Float64b,
            "modifiers" / Array(
                VarInt,
                Struct(
                    "uuid" / String(32),
                    "amount" / Float64b,
                    "operation" / Int8sn,
                ),
            ),
        ),
    ),
),
packet_window_items = Struct(
    "windowId" / Int8un,
    "items" / Array(
        Int16sn,
        Struct(
            "blockId" / Int16sn,
            "anon" / Switch(this.blockId, {
                "-1": Pass,
            }),
        ),
    ),
),
packet_explosion = Struct(
    "x" / Float32b,
    "y" / Float32b,
    "z" / Float32b,
    "radius" / Float32b,
    "affectedBlockOffsets" / Array(
        Int32sn,
        Struct(
            "x" / Int8sn,
            "y" / Int8sn,
            "z" / Int8sn,
        ),
    ),
    "playerMotionX" / Float32b,
    "playerMotionY" / Float32b,
    "playerMotionZ" / Float32b,
),
packet_combat_event = Struct(
    "event" / VarInt,
    "duration" / Switch(this.event, {
        "1": VarInt,
    }),
    "playerId" / Switch(this.event, {
        "2": VarInt,
    }),
    "entityId" / Switch(this.event, {
        "1": Int32sn,
        "2": Int32sn,
    }),
    "message" / Switch(this.event, {
        "2": PascalString(VarInt, "utf-8"),
    }),
),
packet_entity_teleport = Struct(
    "entityId" / VarInt,
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "yaw" / Int8sn,
    "pitch" / Int8sn,
    "onGround" / Flag,
),
packet_statistics = Struct(
    "entries" / Array(
        VarInt,
        Struct(
            "name" / PascalString(VarInt, "utf-8"),
            "value" / VarInt,
        ),
    ),
),
packet_entity_status = Struct(
    "entityId" / Int32sn,
    "entityStatus" / Int8sn,
),
packet_block_action = Struct(
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
    "byte1" / Int8un,
    "byte2" / Int8un,
    "blockId" / VarInt,
),
packet_title = Struct(
    "action" / VarInt,
    "text" / Switch(this.action, {
        "0": PascalString(VarInt, "utf-8"),
        "1": PascalString(VarInt, "utf-8"),
        "2": PascalString(VarInt, "utf-8"),
    }),
    "fadeIn" / Switch(this.action, {
        "3": Int32sn,
    }),
    "stay" / Switch(this.action, {
        "3": Int32sn,
    }),
    "fadeOut" / Switch(this.action, {
        "3": Int32sn,
    }),
),
packet_block_change = Struct(
    "location" / BitStruct(
        "x" / BitsInteger(26, signed=True),
        "y" / BitsInteger(12, signed=True),
        "z" / BitsInteger(26, signed=True),
    ),
    "type" / VarInt,
),
packet_spawn_entity_weather = Struct(
    "entityId" / VarInt,
    "type" / Int8sn,
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
),
packet_map_chunk = Struct(
    "x" / Int32sn,
    "z" / Int32sn,
    "groundUp" / Flag,
    "bitMap" / VarInt,
    "chunkData" / PrefixedBuffer(
        VarInt,
    ),
    "blockEntities" / Array(
        VarInt,
        NBT,
    ),
),
packet_spawn_entity_living = Struct(
    "entityId" / VarInt,
    "entityUUID" / String(32),
    "type" / VarInt,
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "yaw" / Int8sn,
    "pitch" / Int8sn,
    "headPitch" / Int8sn,
    "velocityX" / Int16sn,
    "velocityY" / Int16sn,
    "velocityZ" / Int16sn,
    "metadata" / Pass,  # unfinished type UNKNOWN
),
packet_entity_effect = Struct(
    "entityId" / VarInt,
    "effectId" / Int8sn,
    "amplifier" / Int8sn,
    "duration" / VarInt,
    "hideParticles" / Int8sn,
),
packet_login = Struct(
    "entityId" / Int32sn,
    "gameMode" / Int8un,
    "dimension" / Int32sn,
    "difficulty" / Int8un,
    "maxPlayers" / Int8un,
    "levelType" / PascalString(VarInt, "utf-8"),
    "reducedDebugInfo" / Flag,
),
packet_advancements = Struct(
    "reset" / Flag,
    "advancementMapping" / Array(
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
                            "blockId" / Int16sn,
                            "anon" / Switch(this.blockId, {
                                "-1": Pass,
                            }),
                        ),
                        "frameType" / VarInt,
                        "flags" / BitStruct(
                            "_unused" / BitsInteger(29, signed=False),
                            "hidden" / BitsInteger(1, signed=False),
                            "show_toast" / BitsInteger(1, signed=False),
                            "has_background_texture" / BitsInteger(1, signed=False),
                        ),
                        "backgroundTexture" / Switch(this.flags.has_background_texture, {
                            "1": PascalString(VarInt, "utf-8"),
                        }),
                        "xCord" / Float32b,
                        "yCord" / Float32b,
                    ),
                ),
                "criteria" / Array(
                    VarInt,
                    Struct(
                        "key" / PascalString(VarInt, "utf-8"),
                        "value" / Pass,
                    ),
                ),
                "requirements" / Array(
                    VarInt,
                    Array(
                        VarInt,
                        PascalString(VarInt, "utf-8"),
                    ),
                ),
            ),
        ),
    ),
    "identifiers" / Array(
        VarInt,
        PascalString(VarInt, "utf-8"),
    ),
    "progressMapping" / Array(
        VarInt,
        Struct(
            "key" / PascalString(VarInt, "utf-8"),
            "value" / Array(
                VarInt,
                Struct(
                    "criterionIdentifier" / PascalString(VarInt, "utf-8"),
                    "criterionProgress" / Optional(
                        Int64un,
                    ),
                ),
            ),
        ),
    ),
),
packet_named_entity_spawn = Struct(
    "entityId" / VarInt,
    "playerUUID" / String(32),
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "yaw" / Int8sn,
    "pitch" / Int8sn,
    "metadata" / Pass,  # unfinished type UNKNOWN
),
packet_entity = Struct(
    "entityId" / VarInt,
),
packet_difficulty = Struct(
    "difficulty" / Int8un,
),
packet_update_time = Struct(
    "age" / Int64sn,
    "time" / Int64sn,
),
packet_experience = Struct(
    "experienceBar" / Float32b,
    "level" / VarInt,
    "totalExperience" / VarInt,
),
packet_entity_equipment = Struct(
    "entityId" / VarInt,
    "slot" / VarInt,
    "item" / Struct(
        "blockId" / Int16sn,
        "anon" / Switch(this.blockId, {
            "-1": Pass,
        }),
    ),
),
packet_entity_destroy = Struct(
    "entityIds" / Array(
        VarInt,
        VarInt,
    ),
),
packet_animation = Struct(
    "entityId" / VarInt,
    "animation" / Int8un,
),
packet_rel_entity_move = Struct(
    "entityId" / VarInt,
    "dX" / Int16sn,
    "dY" / Int16sn,
    "dZ" / Int16sn,
    "onGround" / Flag,
),
packet_camera = Struct(
    "cameraId" / VarInt,
),
packet_position = Struct(
    "x" / Float64b,
    "y" / Float64b,
    "z" / Float64b,
    "yaw" / Float32b,
    "pitch" / Float32b,
    "flags" / Int8sn,
    "teleportId" / VarInt,
),
