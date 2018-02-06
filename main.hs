{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import qualified Data.HashMap.Strict as Map
import Data.ByteString.Lazy (readFile, ByteString)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Vector as Vec


data Field = Named Text MCType | Anon MCType deriving (Show)
data MCType = VarInt | U16 | U8 | I64 | Buffer | I32 | I8 | MCBool |
            I16 | F32 | F64 | UUID | 
            Void | RestBuffer | NBT | OptionalNBT
            | Container (Vec.Vector Field)
            | Switch Text (Map.HashMap Text MCType) MCType -- constraint String must be able to cast to value of MCType
            | Bitfield [(Text, Integer, Bool)] -- constrain sum of integers must match lengtoh of type
            | Option MCType
            | PString MCType -- contraint counter must be a numeric type
            | EntityMetadataLoop Integer MCType
            | MCArray MCType
            | Mapper MCType
            deriving (Show)

sourcePath :: String
sourcePath = "minecraft-data/data/pc/1.12.2/protocol.json"

main :: IO ()
main = readFile sourcePath >>= \j -> 
    let jsonData = fromJust $ (decode :: ByteString -> Maybe Object) j in
     print  $ valueToMCType (navigateTo jsonData ["handshaking", "toClient", "types"] Map.! "packet") $ getTypeHashMap jsonData

getTypeHashMap :: Object -> Object
getTypeHashMap o = navigateTo o ["types"] `Map.union` navigateTo o ["play", "toClient", "types"]

valueToMCType :: Value -> Object -> MCType
valueToMCType value typeRef =
    case value of
         String t -> case t of
                          "varint" -> VarInt
                          "u16" -> U16
                          "u8" -> U8
                          "i64" -> I64
                          "buffer" -> Buffer
                          "i32" -> I32
                          "i8" -> I8
                          "bool" -> MCBool
                          "i16" -> I16
                          "f32" -> F32
                          "f64" -> F64
                          "UUID" -> UUID
                          "void" -> Void
                          "restBuffer" -> RestBuffer
                          "nbt" -> NBT
                          "optionalNbt" -> OptionalNBT
                          _ -> valueToMCType (typeRef Map.! t) typeRef
         Array v -> let v0 = fromJust $ v Vec.!? 0
                        v1 = fromJust $ v Vec.!? 1 in
                        case v0 of
                             "container" -> Container (fmap (`valueToField` typeRef) fields)
                                               where fields = case v1 of
                                                                   Array v' -> v'
                                                                   _ -> error "Container fields aren't an array"
                             "mapper" -> Mapper $ valueToMCType v1 typeRef
                             "switch" -> case v1 of
                                 Object o -> case o Map.! "fields" of
                                     Object fields ->
                                         Switch (case o Map.! "compareTo" of
                                                     String t -> t
                                                     _ -> error "compareTo field is not a string")
                                             (fmap (`valueToMCType` typeRef) fields) (case Map.lookup "default" o of
                                                  Just ob -> valueToMCType ob typeRef
                                                  Nothing -> Void)
                                     _ -> error $ "Switch fields should be object not " ++ show o
                             _ -> error "Unknown complex type"
         _ -> U8

valueToField :: Value -> Object -> Field
valueToField value typeRef =
    case value of
         Object o -> if "anon" `Map.member` o
                        then Anon $ valueToMCType (o Map.! "type") typeRef
                        else case o Map.! "name" of
                                  String t -> Named t (valueToMCType (o Map.! "type") typeRef)
                                  _ -> error "Name field is not a string value"
         _ -> error $ "Value " ++ show value ++ " can't be converted to a field"

navigateTo :: Object -> [Text] -> Object
navigateTo o [] = o
navigateTo o (x : xs) =
    case o Map.! x of
         Object ob -> navigateTo ob xs 
         _ -> error ("Could not find: " ++ show x)
