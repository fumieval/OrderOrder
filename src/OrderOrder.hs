{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module OrderOrder (plugin) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Graph qualified as G
import Data.Foldable
import Data.List (intercalate, stripPrefix, sortBy)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Yaml qualified as Yaml
import Text.Dot qualified as Dot

#if MIN_VERSION_ghc(9,0,0)
import GHC.Plugins hiding ((<>))
import GHC.Tc.Utils.Monad (getTopEnv)
import GHC.Unit.Module.Graph
#else
import HscTypes
import Module (moduleName, moduleNameString)
import Plugins
import SrcLoc (GenLocated(..))
import TcRnMonad
#endif

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \_args _ pm -> pure pm
  , pluginRecompile = flagRecompile
  , typeCheckResultAction = \args _ tcGblEnv -> do
    env <- getTopEnv
    case args of
      [stripPrefix "export:" -> Just path] -> liftIO $ do
        let raw = dependencies $ hsc_mod_graph env
        runOrderOrder path raw
      [] -> pure ()
      _ -> error "Usage: -fplugin-opt=OrderOrder:export:/path/to"
    pure tcGblEnv
  }

type Graph = M.Map String (M.Map String Int)
type Summary = M.Map String Graph

runOrderOrder :: FilePath -> Graph -> IO ()
runOrderOrder path raw = do
  let summary = summarise raw
  Yaml.encodeFile (path <> ".yaml") summary
  writeFile (path <> ".dot") $ toDot summary
  writeFile (path <> ".trims.txt")
    $ unlines $ suggestTrims raw $ findFAS <$> summary

relations :: [String] -> [String] -> [(String, String, [String])]
relations [] _ = []
relations _ [] = []
relations (x : xs) (y : ys)
  | x == y = fmap (x:) <$> relations xs ys
  | otherwise = [(x, y, [])]

dependencies :: ModuleGraph -> Graph
dependencies graph = M.fromListWith (<>)
  [ (moduleNameString src, M.singleton (moduleNameString dst) 1)
  | ms <- mgModSummaries graph
  , let src = moduleName $ ms_mod ms
  , (_, L _ dst) <- ms_textual_imps ms
  , dst `Set.member` ourModules
  ]
  where
    ourModules = Set.fromList $ moduleName . ms_mod <$> mgModSummaries graph

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