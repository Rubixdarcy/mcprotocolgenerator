module MCPG.Options (opts, Args(..)) where

import Options.Applicative
import Data.Semigroup ((<>))


data Args = Args
    { repoPath :: FilePath
    , version :: String
    , outPath :: FilePath }

args :: Parser Args
args = Args
    <$> argument str (metavar "REPO" <> help "Minecraft data repositoy" <> action "file")
    <*> argument str (metavar "VERSION" <> help "Protocol version")
    <*> argument str (metavar "OUTPUT" <> help "Output path"
                      <> value "." <> showDefault <> action "file")

opts = info ( args <**> helper )
    ( fullDesc
   <> progDesc "Generate python construct files for the Minecraft Protocol"
    )
