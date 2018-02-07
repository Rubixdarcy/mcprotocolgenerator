{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import qualified Data.HashMap.Strict as Map
import Data.ByteString.Lazy (readFile, ByteString)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Vector as Vec
import Control.Applicative (liftA2, liftA3)
import Data.Foldable (foldl)
import Debug.Trace
import qualified Data.Text.IO as TextIO


data MCField = Named Text MCType | Anon MCType deriving (Show)
data MCType = VarInt | U16 | U8 | I64 | Buffer | I32 | I8 | MCBool |
            I16 | F32 | F64 | UUID | 
            Void | RestBuffer | NBT | OptionalNBT
            | Container [MCField]
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
        case parse getTypeHashMapParser jsonData of
             Success typeRef ->
                 case parse (\jData -> (jData .: "play") >>= (.: "toClient") >>= (.: "types") >>= (.: "packet")) jsonData of
                      Success value ->
                          case parse (parseMCType typeRef) value of
                               Success mcType -> print mcType
--            return $ parseMCType typeData packetData
 --       of 
 --           Success a -> show a

getTypeHashMapParser :: Object -> Parser Object
getTypeHashMapParser root =
    liftA2 Map.union (root .: "types")  (root .: "play" >>= (.: "toClient") >>= (.: "types"))

getItem :: Vec.Vector Value -> Int -> Parser Value
getItem v n = case v Vec.!? n of
                 Just x -> return x
                 Nothing -> typeMismatch ("item " ++ show n) $ Array v

parseMCType :: Object -> Value -> Parser MCType
parseMCType typeRef value =
    case value of
         String t -> case t of
                          "varint"      -> return VarInt
                          "u16"         -> return U16
                          "u8"          -> return U8
                          "i64"         -> return I64
                          "buffer"      -> return Buffer
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
                          _ -> typeRef .: t >>= parseMCType typeRef
         Array v -> do  v0 <- v `getItem` 0
                        v1 <- v `getItem` 1
                        case v0 of
                             "container" ->
                                 (withArray "container field array" $ \fields ->
                                     Container <$> foldl 
                                        (\a b -> liftA2 (:) (parseMCField typeRef b) a) (return []) (Vec.toList fields)
                                 ) v1
                             "mapper" -> Mapper <$> parseMCType typeRef v1
                             "switch" -> (withObject "switch description object" $ \o ->
                                            liftA3 Switch
                                               (o .: "compareTo")
                                               ((o .: "fields") >>= withObject "switch mapping" (parseMap typeRef))
                                               (((o .:? "default") .!= String "void") >>= parseMCType typeRef)
                                         ) v1
                             _ -> return Void -- Unknown complex type
         _ -> return U8

parseMap :: Object -> Object -> Parser (Map.HashMap Text MCType)
parseMap typeRef obj = Map.fromList <$> (foldl
                                (\parser item -> liftA2 (:) ((\p -> (fst item, p)) <$> (parseMCType typeRef $ snd item)) parser)
                                (return [])
                                (Map.toList obj))

parseMCField :: Object -> Value -> Parser MCField
parseMCField typeRef =
    withObject "field object" $ \o ->
        o .:? "anon" >>= \anon ->
            case anon of
                 Just (Data.Aeson.Bool _) -> Anon <$> ((o .: "type") >>= parseMCType typeRef)
                 Nothing -> liftA2 Named (o .: "name") ((o .: "type") >>= parseMCType typeRef)
                 Just v -> typeMismatch "anonymous field" v

renderIndent :: Int -> IO ()
renderIndent n = putStr $ concat $ replicate (n * 4) " "

renderMCType :: Int -> MCType -> IO()
renderMCType n (Container fields) = do
    putStr "Struct (\n"
    sequence_ [renderIndent (n+1) >> renderMCField (n+1) f | f <- fields]
    putStr "\n"
    renderIndent n
    putStr ")\n"
renderMCType _ _ = putStr " # unfinished type \n"

renderMCField :: Int -> MCField -> IO ()
renderMCField n field = do
    renderIndent n
    let (name, mcType) = case field of
                             Named na t -> (na, t)
                             Anon t -> ("anon", t)
    putStr "\""
    TextIO.putStr name
    putStr "\" / "
    renderMCType (n+1) mcType
