module Main where

import Data.ByteString
import Options.Applicative

import MCPG (generateFiles)
import MCPG.Options (opts)


main :: IO ()
main = do
    args <- execParser opts
    generateFiles args

