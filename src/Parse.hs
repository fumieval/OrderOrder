module Parse where

import Data.ByteString.Char8 qualified as B
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import System.Directory
import System.FilePath
import Types

enumSources :: FilePath -> IO [FilePath]
enumSources path = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      paths <- listDirectory path
      mconcat <$> traverse (\x -> map (x </>) <$> enumSources (path </> x)) paths
    else pure [""]

extractImport :: [String] -> [String]
extractImport ("import" : ('"' : _pkg) : "qualified" : name : _) = [name]
extractImport ("import" : ('"' : _pkg) : name : _) = [name]
extractImport ("import" : "qualified" : name : _) = [name]
extractImport ("import" : name : _) = [name]
extractImport _ = []

getImports :: FilePath -> IO [ModuleName]
getImports path = concatMap (fmap (ModuleName . splitOn ".") . extractImport)
  . map words
  . lines
  . B.unpack
  <$> B.readFile path

getEntry :: FilePath -> FilePath -> IO (Graph ModuleName)
getEntry prefix path = do
  let modName = ModuleName $ splitOn "/" $ dropExtension path
  imports <- getImports $ prefix </> path
  pure $ M.singleton modName $ M.fromList $ (ModuleName [], 0) : [ (x, 1) | x <- imports ]
