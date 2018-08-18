
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
import System.FilePath ((</>))
import System.Exit
import Data.Monoid ((<>))

import MCPG.MCType (parseMCType)
import MCPG.Render (renderTopLevel)
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


generateModule :: FilePath -> PacketSet -> Text -> IO ()
generateModule out (PacketSet state direction) content = do
    ensureDir  out
    ensureFile outInit
    ensureDir  stateDir
    ensureFile stateInit
    TextIO.writeFile packetFile content
  where
    outInit    = out </> "__init__.py"
    stateDir   = out </> unpack state
    stateInit  = stateDir </> "__init__.py"
    packetFile = stateDir <> "/" <> (unpack $ directionJSONName direction) <> ".py"
    ensureDir dir = do
        dirExists <- doesDirectoryExist dir
        if not dirExists then createDirectory dir else return ()
    ensureFile file = do
        fileExists <- doesFileExist stateDir
        if not fileExists then TextIO.writeFile file "" else return ()


generateFiles :: Args -> IO ()
generateFiles (Args repo v out) = do
    exists <- doesDirectoryExist repo
    if not exists then do
        putStrLn $ "Could not find directory " ++ repo
        exitWith $ ExitFailure 1
    else return ()
    existsV <- doesDirectoryExist vPath
    if not existsV then do
        putStrLn $ "Could not find version " ++ vPath
        exitWith $ ExitFailure 1
    else return ()
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
mainMCType jsonData out =
    forM_ packetLocations printPackets
  where
    printPackets :: PacketSet -> IO ()
    printPackets ps@(PacketSet state direction) = case do
        typeRef <- parse (getTypeHashMapParser path) jsonData
        value   <- (parse (\jData -> foldM (.:) jData path >>= (.: "packet")) jsonData)
        mcType  <- parse (parseMCType typeRef) value
        return $ generateModule out ps $ renderTopLevel mcType
      of
        Success io -> io
        Error err -> print err
      where
        path =[state, directionJSONName direction, "types"]

getTypeHashMapParser :: [Text] -> Object -> Parser Object
getTypeHashMapParser path root =
    liftA2 Map.union (root .: "types") (foldM (.:) root path)


