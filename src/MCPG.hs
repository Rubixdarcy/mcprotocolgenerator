
{-# LANGUAGE OverloadedStrings #-}

module MCPG
    ( generateFiles
    ) where

import Prelude hiding (readFile)
import qualified Data.HashMap.Strict as Map
import Data.ByteString.Lazy (readFile, ByteString)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromJust)
import Data.Text (Text, unpack, pack)
import qualified Data.Vector as Vec
import Control.Applicative (liftA2, liftA3)
import Control.Monad (void, foldM, forM_)
import qualified Data.Text.IO as TextIO
import Data.List (find)
import Debug.Trace
import Data.List.Split (splitOn)

import MCPG.MCType (parseMCType)
import MCPG.Render (renderTopLevel)





data PacketSet = PacketSet FilePath [Text]
packetLocations :: [PacketSet]
packetLocations =
    [ PacketSet "out_files/play.py"        ["play"       , "toClient", "types"]
    , PacketSet "out_files/handshaking.py" ["handshaking", "toServer", "types"]
    , PacketSet "out_files/login.py"       ["login", "toServer", "types"]
    ]



sourcePath :: String
sourcePath = "minecraft-data/data/pc/1.12.2/protocol.json"

generateFiles :: IO ()
generateFiles = readFile sourcePath >>= \j ->
    let jsonData = fromJust $ (decode :: ByteString -> Maybe Object) j in
        mainMCType jsonData -- >> putStr "\n\n\n" >> mainMapping jsonData

mainMCType :: Object -> IO ()
mainMCType jsonData =
    forM_ packetLocations printPackets
  where
    printPackets :: PacketSet -> IO ()
    printPackets (PacketSet filepath path) =
        case parse (getTypeHashMapParser path) jsonData of
            Success typeRef ->
                case (parse (\jData -> foldM (.:) jData path >>= (.: "packet")) jsonData) of
                    Success value ->
                        case parse (parseMCType typeRef) value of
                            Success mcType -> TextIO.writeFile filepath $ renderTopLevel mcType
                            Error err -> print err
                    Error err -> print err
            _ -> error "Failed to parse types"

getTypeHashMapParser :: [Text] -> Object -> Parser Object
getTypeHashMapParser path root =
    liftA2 Map.union (root .: "types") (foldM (.:) root path)


mainMapping :: Object -> IO ()
mainMapping jsonData =
   case parse (\jData -> let k = (jData .: "play" )  >>= (.: "toClient") >>= (.: "types") >>= (.: "packet") in
                                   ((Vec.! 1) <$> k)
              ) jsonData of
       Success obj -> print (obj :: Value)
       Error e -> print e


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




