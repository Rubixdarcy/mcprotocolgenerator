{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MCPG.FileSet
    ( FileSet(..)
    , createFileSet
    ) where

import Control.Monad (unless, forM_)
import Data.Text
import Data.Text.IO as TIO
import System.Directory
import System.FilePath ((</>), takeDirectory)
import GHC.Base (Monoid)
import Data.Semigroup (Semigroup)


newtype FileSet = FileSet [(FilePath, Text)] deriving (Semigroup, Monoid)


createFileSet :: FilePath -> FileSet -> IO ()
createFileSet base (FileSet set) = withCurrentDirectory base $ forM_ set createFile
  where
      createFile (path, content) = do
          createDirectoryIfMissing True $ takeDirectory path
          TIO.writeFile path content

-- touchDir :: FilePath -> IO ()
-- touchDir dir = do
--     dirExists <- doesDirectoryExist dir
--     unless dirExists $ createDirectory dir

-- touchFile :: FilePath -> IO ()
-- touchFile file = do
--     fileExists <- doesFileExist file
--     unless fileExists $ TIO.writeFile file ""
