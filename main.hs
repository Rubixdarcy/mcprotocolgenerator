{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import qualified Data.HashMap.Strict as Map
import Data.ByteString.Lazy (readFile, ByteString)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import qualified Data.Vector as Vec
import Control.Applicative (liftA2, liftA3)
import Control.Monad (void)
import qualified Data.Text.IO as TextIO
import Data.List (find)
import Debug.Trace
import Data.List.Split (splitOn)


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
            | Mapper MCType
            | UNKNOWN
            deriving (Show)

data PathElement = Literal String | DotDot deriving (Show)

makePath :: String -> [PathElement]
makePath t = makeElement <$> splitOn "/" t
  where
    makeElement ".." = DotDot
    makeElement s = Literal s

renderPath :: [PathElement] -> IO ()
renderPath path = mapM_ renderElement path
  where
    renderElement (Literal s) = putStr $ "." ++ s
    renderElement DotDot      = putStr "._"

sourcePath :: String
sourcePath = "minecraft-data/data/pc/1.12.2/protocol.json"

main :: IO ()
main = readFile sourcePath >>= \j ->
    let jsonData = fromJust $ (decode :: ByteString -> Maybe Object) j in
        mainMCType jsonData -- >> putStr "\n\n\n" >> mainMapping jsonData

mainMCType :: Object -> IO ()
mainMCType jsonData =
        case parse getTypeHashMapParser jsonData of
             Success typeRef ->
                 case parse (\jData -> (jData .: "play") >>= (.: "toClient") >>= (.: "types") >>= (.: "packet")) jsonData of
                      Success value ->
                          case parse (parseMCType typeRef) value of
                               Success mcType -> renderTopLevel mcType
                               Error err -> print err
                      _ -> error "Failed to parse top level"
             _ -> error "Failed to parse top level"

getTypeHashMapParser :: Object -> Parser Object
getTypeHashMapParser root =
    liftA2 Map.union (root .: "types")  (root .: "play" >>= (.: "toClient") >>= (.: "types"))


mainMapping :: Object -> IO ()
mainMapping jsonData =
   case parse (\jData -> let k = (jData .: "play" )  >>= (.: "toClient") >>= (.: "types") >>= (.: "packet") in
                                   ((Vec.! 1) <$> k)
                  --((Vec.! 1) <$> (((Vec.! 0) <$> ((Vec.! 1) <$> k)) >>= (.: "type"))) >>= (.: "mappings")
              ) jsonData of
       Success obj -> print (obj :: Value)
       Error e -> print e

-- .:v :: TOJSON a => Parser Array -> Int -> Parser a
-- .:v arrayParser n = arrayParser >>=

printMapping :: Map.HashMap Text Text -> IO ()
printMapping o = void (sequence_ (printMappingItem <$> Map.toList o))
--printMapping o = sequence_ <$> traverse (\(pid, packet) ->
  --                                     printMappingItem (pid, packet)
    --                           ) (Map.toList o

printMappingItem :: (Text, Text) -> IO()
printMappingItem (key, value) = do
    putStr "    "
    TextIO.putStr key
    putStr ": \""
    TextIO.putStr value
    putStr "\",\\n"

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
         Array v -> do  v0 <- v `getItem` 0
                        v1 <- v `getItem` 1
                        case v0 of
                             "container" ->
                                 (withArray "container field array" $ \fields ->
                                      Container <$> traverse (parseMCField typeRef) (Vec.toList fields)
                                 ) v1
                             "mapper" -> Mapper <$> parseMCType typeRef v1
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

-- RENDERING

renderTopLevel :: MCType -> IO ()
renderTopLevel (Container fields) =
    let m = find (\f ->
                    case f of
                         Named "params" _ -> True
                         _ -> False
                 ) fields in
    case m of
         Just (Named _ (Switch _ packets _)) ->
             sequence_ [do putStr "packet_"
                           TextIO.putStr p
                           putStr " = "
                           renderMCType 0 t
                        | (p, t) <- Map.toList packets]
         _ -> error "Failed to find a valid params field"
renderTopLevel _ = error "Top level object should be a contaianer"

renderIndent :: Int -> IO ()
renderIndent n = putStr $ concat $ replicate (n * 4) " "

hanging :: Int -> MCType -> IO ()
hanging n mcType = renderIndent n >> renderMCType n mcType

closeParen :: Int -> IO ()
closeParen n = renderIndent n >> putStr "),\n"

renderMCType :: Int -> MCType -> IO()
renderMCType n (Container fields) = do
    putStr "Struct(\n"
    sequence_ [renderIndent (n+1) >> renderMCField (n+1) f | f <- fields]
    --putStr "\n"
    renderIndent n
    putStr "),\n"
renderMCType n (Switch on items _) = do -- TODO handle default
    putStr "Switch(this"
    renderPath $ makePath $ unpack on
    putStr ", {\n"
    sequence_ [renderIndent (n+1) >> putStr (show k) >> putStr ": " >> renderMCType (n+1) v | (k, v) <- Map.toList items]
    renderIndent n
    putStr "}),\n"
renderMCType n (Bitfield arr) = do
    putStr "BitStruct(\n"
    sequence_ (( \(name, size, signed) ->
        do renderIndent (n+1)
           putStr "\""
           TextIO.putStr name
           putStr $ "\" / BitsInteger(" ++ show size ++ ", signed=" ++ show signed ++ "),\n"
        ) <$> arr)
    renderIndent n
    putStr "),\n"
renderMCType _ (PString _) = putStr "PascalString(VarInt, \"utf-8\"),\n"
renderMCType _ F32    = putStr "Float32b,\n"
renderMCType _ F64    = putStr "Float64b,\n"
renderMCType _ U8     = putStr "Int8un,\n"
renderMCType _ U16    = putStr "Int16un,\n"
renderMCType _ U64    = putStr "Int64un,\n"
renderMCType _ I8     = putStr "Int8sn,\n"
renderMCType _ I16    = putStr "Int16sn,\n"
renderMCType _ I32    = putStr "Int32sn,\n"
renderMCType _ I64    = putStr "Int64sn,\n"
renderMCType _ Void   = putStr "Pass,\n"
renderMCType _ VarInt = putStr "VarInt,\n"
renderMCType _ MCBool = putStr "Flag,\n"
renderMCType _ UUID   = putStr "String(32),\n"
renderMCType _ NBT    = putStr "NBT,\n"
renderMCType _ OptionalNBT = putStr "Select(Value(b\"\\x00\"), NBT),\n"
renderMCType n (Buffer mcType) = do
    putStr "PrefixedBuffer(\n"
    hanging (n+1) mcType
    closeParen n
renderMCType n (MCArray countType dataType) = do
    putStr "Array(\n"
    hanging (n+1) countType
    hanging (n+1) dataType
    closeParen n
renderMCType n (Option mcType) = do
    putStr "Optional(\n"
    hanging (n+1) mcType
    closeParen n
renderMCType _ mcType = putStr $ "Pass,  # unfinished type " ++ take 10 (show mcType) ++ "\n"

renderMCField :: Int -> MCField -> IO ()
renderMCField n field = do
    let (name, mcType) = case field of
                             Named na t -> (na, t)
                             Anon t -> ("anon", t)
    putStr "\""
    TextIO.putStr name
    putStr "\" / "
    renderMCType n mcType
