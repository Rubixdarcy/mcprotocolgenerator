{-# LANGUAGE OverloadedStrings #-}

module MCPG.Render where

import MCPG.MCType (MCType(..), PathElement(..), makePath, MCField(..))
import Data.Text hiding (take, replicate, find, last)
import Data.Text.Read (signed, decimal)
import Text.Casing (quietSnake)
import qualified Data.HashMap.Strict as Map
import Data.Monoid ((<>))
import Data.List

renderPathDir :: [PathElement] -> Text
-- Cheating and assuming there is only one exceptional case
--renderPath [DotDot, Literal s] = "._._." <> pack s
renderPathDir p = foldMap renderElement p
  where
    renderElement :: PathElement -> Text
    renderElement (Literal s) = ""
    renderElement DotDot      = "._"

renderPathName :: [PathElement] -> Text
renderPathName p@(h:_) = case last p of
    Literal s -> pack s
    DotDot -> "<PATH WITH NO NAME>"


renderTopLevel :: MCType -> Text
renderTopLevel t = header
                <> renderPackets t
                <> "\n\n\n"
                <> renderID2Struct t
                <> renderID2Name   t
                <> renderName2ID


fieldNamed :: Text -> [MCField] -> Maybe MCField
fieldNamed n = find f
  where
    f (Named n' _) = n == n'
    f _            = False

renderPackets :: MCType -> Text
renderPackets (Container fields) =
    case fieldNamed "params" fields of
         Just (Named _ (Switch _ packets _)) ->
             mconcat [mconcat
                 [ "packet_"
                 ,  p
                 ,  ", _ = ("
                 ,  renderMCType 0 t
                 ,  "None)\n\n\n"
                 ]
             | (p, t) <- Map.toList packets]
         _ -> error "Failed to find a valid params field"
renderPackets _ = error "Top level object should be a contaianer"

renderID2Struct :: MCType -> Text
renderID2Struct (Container fields) =
    case fieldNamed "name" fields of
        Just (Named _ (Mapper _ hm)) -> renderMapping "id2struct" ((\t -> "packet_" <> t) <$> hm)
        Just _             -> "The object named 'mapper' was not a mapper type"
        _                  -> "There was no field named 'mapper'"
renderID2Name :: MCType -> Text
renderID2Name (Container fields) =
    case fieldNamed "name" fields of
        Just (Named _ (Mapper _ hm)) -> renderMapping "id2name" ((\t -> "\"packet_" <> t <> "\"") <$> hm)
        Just _             -> "The object named 'mapper' was not a mapper type"
        _                  -> "There was no field named 'mapper'"
renderName2ID :: Text
renderName2ID = "name2id = {v: k for k, v in id2name.items()}\n"

renderIndent :: Int -> Text
renderIndent n = mconcat $ replicate (n * 4) " "

hanging :: Int -> MCType -> Text
hanging n mcType = renderIndent n <> renderMCType n mcType

closeParen :: Int -> Text
closeParen n = renderIndent n <> "),\n"

renderMCType :: Int -> MCType -> Text
renderMCType n (Container fields) = mconcat
    [ "Struct(\n"
    , mconcat [renderIndent (n+1) <> renderMCField (n+1) f | f <- fields]
      --putStr "\n"
    , renderIndent n
    , "),\n" ]
renderMCType n (Switch on items def) = mconcat
    [ "Switch(\n"
    , renderIndent (n+1)
    , "lambda ctx: ctx", pathDir, ".", pathName 
    , " if \"" , pathName , "\" in ctx", pathDir, " else "
    , "ctx", pathDir, "._.", pathName
    , ",\n"
    , renderIndent (n+1)
    , "{\n"
    , mconcat [renderIndent (n+2) <> asSwitchItem k <> ": " <> renderMCType (n+2) v | (k, v) <- Map.toList items]
    , renderIndent (n+1)
    , "},\n"
    , renderIndent (n+1)
    , "default="
    , renderMCType (n+1) def
    , closeParen n ]
  where
    path = makePath $ quietSnake $ unpack on
    pathDir = renderPathDir path
    pathName = renderPathName path
    asSwitchItem k = case signed decimal k of
        Right _ -> k
        Left  _ -> "\"" <> k <> "\""
renderMCType n (Bitfield arr) = mconcat
    [ "BitStruct(\n"
    , foldMap (\(name, size, signed) -> mconcat
        [ renderIndent (n+1)
        , "\""
        , name
        , "\" / BitsInteger(" <> pack (show size) <> ", signed=" <> pack (show signed) <> "),\n"
        ]
        ) arr
    , renderIndent n
    , "),\n" ]
renderMCType _ (PString _) = "PascalString(VarInt, \"utf-8\"),\n"
renderMCType _ F32         = "Float32b,\n"
renderMCType _ F64         = "Float64b,\n"
renderMCType _ U8          = "Int8ub,\n"
renderMCType _ U16         = "Int16ub,\n"
renderMCType _ U64         = "Int64ub,\n"
renderMCType _ I8          = "Int8sb,\n"
renderMCType _ I16         = "Int16sb,\n"
renderMCType _ I32         = "Int32sb,\n"
renderMCType _ I64         = "Int64sb,\n"
renderMCType _ Void        = "Pass,\n"
renderMCType _ VarInt      = "VarInt,\n"
renderMCType _ MCBool      = "Flag,\n"
renderMCType _ UUID        = "Bytes(16),\n"
renderMCType _ NBT         = "NBT,\n"
renderMCType _ OptionalNBT = "Select(Const(b\"\\x00\"), NBT),\n"
renderMCType n (Buffer mcType) = mconcat
    [ "PrefixedBuffer(\n"
    , hanging (n+1) mcType
    , closeParen n ]
renderMCType n (MCArray countType dataType) = mconcat
    [ "PrefixedArray(\n"
    , hanging (n+1) countType
    , hanging (n+1) dataType
    , closeParen n ]
renderMCType n (Option mcType) = mconcat
    [ "Optional(\n"
    , hanging (n+1) mcType
    , closeParen n ]
renderMCType _ mcType = "Pass,  # unfinished type "
               `mappend` (pack $ take 10 (show mcType))
               `mappend` "\n"

renderMCField :: Int -> MCField -> Text
renderMCField n field = mconcat
    [ "\""
    , name
    , "\" / "
    , renderMCType n mcType
    ]
  where
    (name, mcType) = case field of
        Named na t -> (pack $ quietSnake $ unpack na, t)
        Anon t -> ("anon", t)


renderMapping :: Text -> Map.HashMap Text Text -> Text
renderMapping name mapping =
    name <> " = {\n" <> mconcat (renderMappingItem <$> Map.toList mapping) <> "}\n"

renderMappingItem :: (Text, Text) -> Text
renderMappingItem (key, value) = "    " <> key <> ": " <> value <> ",\n"


header :: Text
header = "from construct import *\n\
         \from ...NBTConstruct import NBT\n\
         \from ...MCStructs import *\n\
         \\n\n"
