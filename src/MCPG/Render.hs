{-# LANGUAGE OverloadedStrings #-}

module MCPG.Render where

import MCPG.MCType (MCType(..), PathElement(..), makePath, MCField(..)) 
import Data.Text hiding (take, replicate, find)
import qualified Data.HashMap.Strict as Map
import Data.Monoid ((<>))
import Data.List

renderPath :: [PathElement] -> Text
renderPath = foldMap renderElement
  where
    renderElement :: PathElement -> Text
    renderElement (Literal s) = "." <> pack s
    renderElement DotDot      = "._"


renderTopLevel :: MCType -> Text
renderTopLevel (Container fields) =
    let m = find (\f ->
                    case f of
                         Named "params" _ -> True
                         _ -> False
                 ) fields in
    case m of
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
renderTopLevel _ = error "Top level object should be a contaianer"

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
    , "this"
    , renderPath $ makePath $ unpack on
    , ",\n"
    , renderIndent (n+1)
    , "{\n"
    , mconcat [renderIndent (n+2) <> k <> ": " <> renderMCType (n+2) v | (k, v) <- Map.toList items]
    , renderIndent (n+1)
    , "},\n"
    , renderIndent (n+1)
    , "default="
    , renderMCType (n+1) def
    , closeParen n ]
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
renderMCType _ UUID        = "PaddedString(16, \"utf8\"),\n"
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
        Named na t -> (na, t)
        Anon t -> ("anon", t)
