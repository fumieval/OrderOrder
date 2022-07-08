{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import Control.Monad
import Data.ByteString.Char8 qualified as B
import Data.Foldable
import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Yaml qualified as Yaml
import Options.Applicative qualified as O
import System.Directory
import System.FilePath
import Text.Dot qualified as Dot

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

getImports :: FilePath -> IO [String]
getImports path = concatMap extractImport . map words . lines . B.unpack <$> B.readFile path

getEntry :: FilePath -> FilePath -> IO Graph
getEntry prefix path = do
  let modName = intercalate "." $ splitOn "/" $ dropExtension path
  imports <- getImports $ prefix </> path
  pure $ M.singleton modName $ M.fromList $ ("", 0) : [ (x, 1) | x <- imports ]

-- drop external modules
prune :: Graph -> Graph
prune graph = M.filterWithKey (\k _ -> M.member k graph) <$> graph

main :: IO ()
main = join $ O.execParser $ flip O.info mempty $ do
  dumpSummary <- O.switch $ O.long "summary" <> O.help "Dump summarised dependencies"
  dumpDot <- O.switch $ O.long "dot" <> O.help "Dump dot representation of the summary"
  dumpFAS <- O.switch $ O.long "fas" <> O.help "Dump a feedback arc set"
  sourceDirs <- O.some $ O.strArgument $ O.metavar "DIR"
  _ <- O.helper
  pure $ do
    graph <- fmap mconcat $ forM sourceDirs $ \dir -> do
      srcs <- enumSources dir
      mconcat <$> traverse (getEntry dir) srcs
    let pruned = prune graph
    let summary = summarise pruned
    when dumpSummary $ B.putStrLn $ Yaml.encode summary
    when dumpDot $ putStrLn $ toDot summary
    when dumpFAS $ putStr $ unlines $ suggestTrims pruned $ findFAS <$> summary

type Graph = M.Map String (M.Map String Int)
type Summary = M.Map String Graph

relations :: [String] -> [String] -> [(String, String, [String])]
relations [] _ = []
relations _ [] = []
relations (x : xs) (y : ys)
  | x == y = fmap (x:) <$> relations xs ys
  | otherwise = [(x, y, [])]

summarise :: Graph -> Summary
summarise graph = M.fromListWith (M.unionWith (M.unionWith (+)))
  [ el
  | (src, dsts) <- M.toList graph
  , (dst, weight) <- M.toList dsts
  , (k, d, p) <- relations (splitOn "." src) (splitOn "." dst)
  , let prefix = intercalate "." p
  , el <- [(prefix, M.singleton k $ M.singleton d weight), (prefix, M.singleton d mempty)]
  ]

type Connectivity a = M.Map a (Set.Set a)

isConnected :: Ord a => Connectivity a -> a -> a -> Bool
isConnected conn src dst = case M.lookup src conn of
  Just set -> Set.member dst set || any (\x -> isConnected conn x dst) set
  Nothing -> False

insertConn :: Ord a => a -> a -> Connectivity a -> Connectivity a
insertConn src dst = M.insertWith (<>) src (Set.singleton dst)

-- | Find a feedback arc set (a set of edges that would eliminate SCCs from the given graph)
findFAS :: Graph -> Graph
findFAS graph = snd $ foldl' step (mempty, mempty) allEdges
  where
    step (conn, acc) (src, dst)
      | isConnected conn dst src = (conn, M.insertWith (<>) src (M.singleton dst 1) acc)
      | otherwise = (insertConn src dst conn, acc)
    allEdges = map fst $ sortBy (flip $ comparing snd) $
      [ ((src, dst), weight)
      | (src, dsts) <- M.toList graph
      , (dst, weight) <- M.toList dsts
      ]

-- Instantiate concrete module names from the given FAS
suggestTrims :: Graph -> M.Map String Graph -> [String]
suggestTrims rawGraph trimMap =
  [ unwords [src, "->", dst]
  | (src, dsts) <- M.toList rawGraph
  , dst <- M.keys dsts
  , (k, d, prefix) <- relations (splitOn "." src) (splitOn "." dst)
  , trims <- toList $ M.lookup (intercalate "." prefix) trimMap
  , ds <- toList $ M.lookup k trims
  , M.member d ds
  ]

cluster :: Graph -> Dot.Dot ()
cluster group = do
  nodes <- M.traverseWithKey (\k vs -> (,) vs <$> Dot.node [("label", k)]) group
  forM_ (M.elems nodes) $ \(vs, node) -> do
    forM_ (M.toList vs) $ \(v, weight) -> forM_ (M.lookup v nodes)
      $ \(_, n') -> Dot.edge node n' [("weight", show weight), ("label", show weight)]

toDot :: Summary -> String
toDot summary = Dot.showDot $ do
  Dot.attribute ("rankdir", "LR")
  forM_ (M.toList summary) $ \(prefix, group) -> Dot.cluster $ do
    Dot.attribute ("label", prefix)
    cluster group