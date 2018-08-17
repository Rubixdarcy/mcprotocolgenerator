{-# LANGUAGE OverloadedStrings #-}

module MCPG.MCType
    ( MCType(..)
    , parseMCType
    , PathElement(..)
    , makePath
    , MCField(..)
    ) where

import Data.Text (Text)
import qualified Data.HashMap.Strict as Map
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as Vec
import Debug.Trace
import Control.Applicative (liftA3, liftA2)
import Data.List.Split
import Control.Monad ((>=>))

data MCField = Named Text MCType | Anon MCType deriving (Show)
data MCType = VarInt | U16 | U8 | I64 | I32 | I8 | MCBool |
            I16 | F32 | F64 | UUID | U64 |
            Void | RestBuffer | NBT | OptionalNBT
            | Buffer MCType
            | Container [MCField]
            | Switch Text (Map.HashMap Text MCType) MCType -- constraint String must be able to cast to value of MCType
            | Bitfield [(Text, Integer, Bool)] -- constrain sum of integers must match lengtoh of type
            | Option MCType
            | PString MCType -- contraint counter must be a numeric type
            | EntityMetadataLoop Integer MCType
            | MCArray MCType MCType
            | Mapper MCType (Map.HashMap Text Text)
            | UNKNOWN
            deriving (Show)



parseMCType :: Object -> Value -> Parser MCType
parseMCType typeRef value =
    case value of
         String t -> case t of
             "varint"      -> return VarInt
             "u16"         -> return U16
             "u8"          -> return U8
             "i64"         -> return I64
             "i32"         -> return I32
             "i8"          -> return I8
             "bool"        -> return MCBool
             "i16"         -> return I16
             "f32"         -> return F32
             "f64"         -> return F64
             "UUID"        -> return UUID
             "void"        -> return Void
             "restBuffer"  -> return RestBuffer
             "nbt"         -> return NBT
             "optionalNbt" -> return OptionalNBT
             "u64"         -> return U64
             _ -> typeRef .: t >>= parseMCType typeRef
         Array v -> do
             v0 <- v `getItem` 0
             v1 <- v `getItem` 1
             case v0 of
                  "container" ->
                      (withArray "container field array" $ \fields ->
                           Container <$> traverse (parseMCField typeRef) (Vec.toList fields)
                      ) v1
                  "mapper" -> (liftA2 Mapper) ((withObject "omap" (.: "type") v1 ) >>= parseMCType typeRef)
                                              ((withObject "omap" ((.: "mappings") >=> parseJSON) v1 ))
                  "switch" -> (withObject "switch description object" $ \o ->
                                 liftA3 Switch
                                    (o .: "compareTo")
                                    ((o .: "fields") >>= withObject "switch mapping" (traverse (parseMCType typeRef)))
                                    (((o .:? "default") .!= String "void") >>= parseMCType typeRef)
                              ) v1
                  "pstring" -> PString <$> parseMCType typeRef v1
                  "array" -> withObject "array types object" (\o ->
                      do countType <- ((o .:? "countType") .!= String "varint") >>= parseMCType typeRef
                         dataType <- o .: "type" >>= parseMCType typeRef
                         return $ MCArray countType dataType
                      ) v1
                  "bitfield" -> withArray "bitfield array" handleBitfield v1
                  "option" -> Option <$> parseMCType typeRef v1
                  "buffer" -> Buffer <$> withObject "buffer args" (\o -> o .: "countType" >>= parseMCType typeRef) v1
                  _ -> return UNKNOWN -- Unknown complex type
         x -> return $ trace ("<unknown: " ++ show x ++ ">") UNKNOWN





getItem :: Vec.Vector Value -> Int -> Parser Value
getItem v n = case v Vec.!? n of
                 Just x -> return x
                 Nothing -> typeMismatch ("item " ++ show n) $ Array v

handleBitfield :: Array -> Parser MCType
handleBitfield arr = Bitfield <$> traverse (withObject "bitfield object" (\field ->
                               do name   <- field .: "name"
                                  size   <- field .: "size"
                                  signed <- field .: "signed"
                                  return (name, size, signed)
                              )) (Vec.toList arr)

parseMCField :: Object -> Value -> Parser MCField
parseMCField typeRef =
    withObject "field object" $ \o ->
        o .:? "anon" >>= \anon ->
            case anon of
                 Just (Data.Aeson.Bool _) -> Anon <$> ((o .: "type") >>= parseMCType typeRef)
                 Nothing -> liftA2 Named (o .: "name") ((o .: "type") >>= parseMCType typeRef)
                 Just v -> typeMismatch "anonymous field" v


data PathElement = Literal String | DotDot deriving (Show)

makePath :: String -> [PathElement]
makePath t = makeElement <$> splitOn "/" t
  where
    makeElement :: String -> PathElement
    makeElement ".." = DotDot
    makeElement s = Literal s
