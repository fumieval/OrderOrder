{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module OrderOrder (plugin) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Graph qualified as G
import Data.List (intercalate, stripPrefix)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
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
        let summary = summarise raw
        let sorted = sortGroup <$> summary
        Yaml.encodeFile (path <> ".yaml") sorted
        writeFile (path <> ".dot") $ toDot summary
      [] -> pure ()
      _ -> error "Usage: -fplugin-opt=OrderOrder:export:/path/to"
    pure tcGblEnv
  }

type Graph = M.Map String (M.Map String Int)
type Summary = M.Map String Graph

relations :: [String] -> [String] -> [(String, String, [String])]
relations [] _ = []
relations _ [] = []
relations (x : xs) (y : ys)
  | x == y = fmap (x:) <$> relations xs ys
  | otherwise = [(x, y, [])]

sortGroup :: Graph -> [String]
sortGroup group = [ k | (k, _, _) <- map nodeFromVertex $ G.reverseTopSort graph ] where
  (graph, nodeFromVertex, _vertexFromKey) = G.graphFromEdges
    [ (k, k, M.keys vs) | (k, vs) <- M.toList group]

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

cluster :: Graph -> Dot.Dot ()
cluster group = do
  nodes <- M.traverseWithKey (\k vs -> (,) vs <$> Dot.node [("label", k)]) group
  forM_ (M.elems nodes) $ \(vs, node) -> do
    forM_ (M.toList vs) $ \(v, weight) -> forM_ (M.lookup v nodes)
      $ \(_, n') -> Dot.edge node n' [("weight", show weight)]

toDot :: Summary -> String
toDot summary = Dot.showDot $ do
  Dot.attribute ("rankdir", "LR")
  forM_ (M.toList summary) $ \(prefix, group) -> Dot.cluster $ do
    Dot.attribute ("label", prefix)
    cluster group