
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
import Control.Monad (void, foldM, forM_, unless)
import qualified Data.Text.IO as TextIO
import Data.List (find)
import Debug.Trace
import Data.List.Split (splitOn)
import System.Directory
import System.FilePath ((</>))
import System.Exit
import Data.Monoid ((<>))

import MCPG.MCType (parseMCType)
import MCPG.RenderPython (renderTopLevel)
import MCPG.FileSet (FileSet(..), createFileSet)
import MCPG.Options (Args(..))



data PacketDirection = ToClient | ToServer
directionJSONName :: PacketDirection -> Text
directionJSONName ToClient = "toClient"
directionJSONName ToServer = "toServer"

data PacketSet = PacketSet Text PacketDirection
packetLocations :: [PacketSet]
packetLocations =
    [ PacketSet "play"        ToClient
    , PacketSet "play"        ToServer
    , PacketSet "handshaking" ToClient
    , PacketSet "handshaking" ToServer
    , PacketSet "login"       ToClient
    , PacketSet "login"       ToServer
    ]



sourcePath :: String
sourcePath = "minecraft-data/data/pc/1.12.2/protocol.json"


generateModule :: PacketSet -> Text -> FileSet
generateModule (PacketSet state direction) content =
    FileSet
        [ ("__init__.py", "")
        , (stateDir </> "__init__.py", "")
        , (stateDir </> packetFile, content)
        ]
  where
    stateDir   = unpack state
    packetFile = unpack (directionJSONName direction) <> ".py"


generateFiles :: Args -> IO ()
generateFiles (Args repo v out) = do

    exists <- doesDirectoryExist repo
    unless exists $ do
        putStrLn $ "Could not find directory " ++ repo
        exitWith $ ExitFailure 1

    existsV <- doesDirectoryExist vPath
    unless existsV $ do
        putStrLn $ "Could not find version " ++ vPath
        exitWith $ ExitFailure 1

    j <- readFile $ vPath </> "protocol.json"
    let jsonData = fromJust $ (decode :: ByteString -> Maybe Object) j
    mainMCType jsonData (out </> vPythonName)
  where
    vPath = repo </> "data/pc/" </> v
    vPythonName = "v" ++ map repl v
      where
        repl '.' = '_'
        repl c   = c


mainMCType :: Object -> FilePath -> IO ()
mainMCType jsonData out = do
    createDirectoryIfMissing False out
    forM_ packetLocations printPackets
  where
    printPackets :: PacketSet -> IO ()
    printPackets ps@(PacketSet state direction) = case do
        typeRef <- parse (getTypeHashMapParser path) jsonData
        value   <- parse (\jData -> foldM (.:) jData path >>= (.: "packet")) jsonData
        mcType  <- parse (parseMCType typeRef) value
        return $ createFileSet out $ generateModule ps $ renderTopLevel mcType
      of
        Success io -> io
        Error err -> print err
      where
        path = [state, directionJSONName direction, "types"]

getTypeHashMapParser :: [Text] -> Object -> Parser Object
getTypeHashMapParser path root =
    liftA2 Map.union (root .: "types") (foldM (.:) root path)


