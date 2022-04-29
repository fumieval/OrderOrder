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
import DynFlags
import GHC.Hs (HsModule(..), ideclName)
import HscTypes
import Module (ModuleName, moduleName, pprModuleName, moduleNameString)
import Outputable ((<+>), ppr, printForUser, alwaysQualify, defaultUserStyle)
import Plugins
import SrcLoc (SrcSpan, GenLocated(..))
import TcRnMonad
#endif

moduleKey :: ModuleName -> [String]
moduleKey = splitOn "." . moduleNameString

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \_args _ pm -> pure pm
  , pluginRecompile = flagRecompile
  , typeCheckResultAction = \args _ tcGblEnv -> do
    env <- getTopEnv
    case args of
      [stripPrefix "export:" -> Just path] -> do
        let summary = summarise $ hsc_mod_graph env
        liftIO $ Yaml.encodeFile path $ sortGroup <$> summary
        liftIO $ writeFile (path <> ".dot") $ toDot summary
      [] -> pure ()
      _ -> error "Usage: -fplugin-opt=OrderOrder:export:/path/to"
    pure tcGblEnv
  }

type Group = M.Map String (Set.Set String)
type Summary = M.Map String Group

relations :: [String] -> [String] -> [(String, String, [String])]
relations [] _ = []
relations _ [] = []
relations (x : xs) (y : ys)
  | x == y = fmap (x:) <$> relations xs ys
  | otherwise = [(x, y, [])]

sortGroup :: Group -> [String]
sortGroup group = [ k | (k, _, _) <- map nodeFromVertex $ G.reverseTopSort graph ] where
  (graph, nodeFromVertex, _vertexFromKey) = G.graphFromEdges
    [ (k, k, Set.toList vs) | (k, vs) <- M.toList group]

summarise :: ModuleGraph -> Summary
summarise graph = M.fromListWith (M.unionWith (<>))
    [ el
    | ms <- mgModSummaries graph
    , let src = moduleKey $ moduleName $ ms_mod ms
    , (_, L _ m) <- ms_textual_imps ms
    , let dst = moduleKey m
    , dst `Set.member` ourModules
    , (k, d, p) <- relations src dst
    , let prefix = intercalate "." p
    , el <- [(prefix, M.singleton k $ Set.singleton d), (prefix, M.singleton d mempty)]
    ]
  where
    ourModules = Set.fromList $ moduleKey . moduleName . ms_mod <$> mgModSummaries graph

toDot :: Summary -> String
toDot summary = Dot.showDot $ do
  Dot.attribute ("rankdir", "LR")
  forM_ (M.toList summary) $ \(prefix, group) -> Dot.scope $ do
    Dot.attribute ("label", prefix)
    nodes <- M.traverseWithKey (\k vs -> (,) vs <$> Dot.node [("label", k)]) group
    forM_ (M.elems nodes) $ \(vs, node) -> do
      forM_ vs $ \v -> forM_ (M.lookup v nodes) $ \(_, n') -> node Dot..->. n'