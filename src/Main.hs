module Main where

import System.Environment (getArgs)
import Text.Regex.TDFA ((=~))
import System.Path (fileList, filterUseless)
import Control.Monad (filterM, forM_)
import System.FilePath ((</>))
import System.Directory (renameDirectory, getCurrentDirectory, getDirectoryContents, createDirectory, doesDirectoryExist)
import Data.List (isSuffixOf)
import System.FilePath (dropFileName)
import qualified System.IO.Strict as SIO

replaceText :: String -> (String -> String) -> String -> String
replaceText re sub text
  | null match = before 
  | otherwise = before ++ sub match ++ replaceText re sub after
  where (before, match, after) = text =~ re :: (String, String, String)

cljList :: IO [FilePath]
cljList = do
  current <- getCurrentDirectory
  src     <- fileList $ current </> "src"
  test    <- fileList $ current </> "test"
  return $ filter (isSuffixOf ".clj") $ src ++ test

firstUsefulDir :: FilePath -> [FilePath] -> FilePath
firstUsefulDir path = (path </>) . head . filterUseless

moveDirs :: FilePath -> FilePath -> IO ()
moveDirs old new = do
  current <- getCurrentDirectory
  let newSrc  = current </> "src" </> new
      newTest = current </> "test" </> new
      oldTest = current </> "test" </> old
  createDirectory newSrc
  createDirectory newTest
  renameDirectory (current </> "src" </> old) (newSrc </> old)
  exists <- doesDirectoryExist oldTest
  if exists
    then renameDirectory oldTest (newTest </> old)
    else return ()

prefixFiles :: String -> String -> IO ()
prefixFiles re prefix = do
  files <- cljList
  forM_ files $ \file -> do
    contents <- SIO.readFile file
    writeFile file $ replaceText (re ++ "\\.[a-zA-Z._-]+") sub contents
  where sub text = prefix ++ "." ++ text
  
main :: IO ()
main = do
  args <- getArgs
  case args of
    [old, prefix]              -> prefixFiles old prefix >> moveDirs old prefix
    ["--no-move", old, prefix] -> prefixFiles old prefix
