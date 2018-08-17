
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
import System.Directory
import Data.Monoid ((<>))

import MCPG.MCType (parseMCType)
import MCPG.Render (renderTopLevel)



data PacketDirection = ToClient | ToServer
directionJSONName :: PacketDirection -> Text
directionJSONName ToClient = "toClient"
directionJSONName ToServer = "toServer"

data PacketSet = PacketSet Text PacketDirection
packetLocations :: [PacketSet]
packetLocations =
    [ PacketSet "play" ToClient
    -- , PacketSet "out_files/handshaking.py" ["handshaking", "toServer", "types"]
    -- , PacketSet "out_files/login.py"       ["login", "toServer", "types"]
    ]



sourcePath :: String
sourcePath = "minecraft-data/data/pc/1.12.2/protocol.json"


generateModule :: PacketSet -> Text -> IO ()
generateModule (PacketSet state direction) content = do
    ensureDir stateDir
    ensureFile stateInit
    TextIO.writeFile packetFile content
  where
    stateDir   = "out_files/" ++ unpack state
    stateInit  = stateDir <> "/.__init__.py"
    packetFile = stateDir <> "/" <> (unpack $ directionJSONName direction) <> ".py"
    ensureDir dir = do
        dirExists <- doesDirectoryExist dir
        if not dirExists then createDirectory dir else return ()
    ensureFile file = do
        fileExists <- doesFileExist stateDir
        if not fileExists then TextIO.writeFile file "" else return ()


generateFiles :: IO ()
generateFiles = readFile sourcePath >>= \j ->
    let jsonData = fromJust $ (decode :: ByteString -> Maybe Object) j in
        mainMCType jsonData -- >> putStr "\n\n\n" >> mainMapping jsonData

mainMCType :: Object -> IO ()
mainMCType jsonData =
    forM_ packetLocations printPackets
  where
    printPackets :: PacketSet -> IO ()
    printPackets ps@(PacketSet state direction) = case do
        typeRef <- parse (getTypeHashMapParser path) jsonData
        value <- (parse (\jData -> foldM (.:) jData path >>= (.: "packet")) jsonData)
        mcType <- parse (parseMCType typeRef) value
        return $ generateModule ps $ renderTopLevel mcType
      of
        Success io -> io
        Error err -> print err
      where
        path =[state, directionJSONName direction, "types"]

getTypeHashMapParser :: [Text] -> Object -> Parser Object
getTypeHashMapParser path root =
    liftA2 Map.union (root .: "types") (foldM (.:) root path)


