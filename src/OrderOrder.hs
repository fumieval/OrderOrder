{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module OrderOrder (plugin) where

import Control.Monad
import Data.Maybe (isNothing)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Foldable
import Data.Yaml qualified as Yaml
import GHC.Data.Bag (listToBag)
import GHC.Hs (hsmodImports, ImportDecl(..), HsParsedModule(..), HsModule(..), SrcSpanAnn'(..))
import GHC.Plugins hiding ((<>))
import GHC.Types.Error
import GHC.Tc.Utils.Monad (getTopEnv, tcg_mod)
import GHC.Unit.Module.Graph
import System.Directory
import PrefixMap qualified as PM

moduleKey :: ModuleName -> [String]
moduleKey = splitOn "." . moduleNameString

listImports :: HsModule -> [(SrcSpan, ModuleName, ModuleName)]
listImports hsmod = [(locA loc, this, that)
  | L _ this <- maybe [] pure $ hsmodName hsmod
  , L loc that' <- hsmodImports hsmod
  , let L _ that = ideclName that'
  ]

summaryFileName :: FilePath
summaryFileName = "import-summary.yaml"

verify :: HsParsedModule -> Summary -> Hsc ()
verify pm rawMapping = do
  let mapping = mconcat
        [ PM.singleton (splitOn "." k) $ PM.singleton (splitOn "." v) ()
        | (k, vs) <- M.toList rawMapping
        , v <- vs
        ]
  let L _ hsmod = hpm_module pm
  let problems = [(loc, this, that)
        | (loc, this, that) <- listImports hsmod
        , m <- toList $ PM.lookup (moduleKey that) mapping
        , () <- toList $ PM.lookup (moduleKey this) m
        , isNothing
          $ PM.lookup (moduleKey this) mapping
          >>= PM.lookup (moduleKey that)
        ]
  unless (null problems) $ throwErrors $ listToBag
    [ mkPlainMsgEnvelope loc
    $ pprModuleName this <+> "imports" <+> pprModuleName that
    | (loc, this, that) <- problems]

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \_args _ pm -> do
    exist <- liftIO $ doesFileExist summaryFileName
    if exist
      then Yaml.decodeFileThrow summaryFileName >>= verify pm
      else pure ()
    pure pm
  , pluginRecompile = flagRecompile
  , renamedResultAction = \_args tcGblEnv ghcRn -> do
    env <- getTopEnv
    dynFlags <- getDynFlags
    when (moduleName (tcg_mod tcGblEnv) == mainModuleNameIs dynFlags)
      $ liftIO
      $ Yaml.encodeFile summaryFileName
      $ summarise $ hsc_mod_graph env

    pure (tcGblEnv, ghcRn)
  }

type Summary = M.Map String [String]

summarise :: ModuleGraph -> Summary
summarise graph = M.fromList
  $ map (\(k, v) -> (intercalate "." k, [ intercalate "." j | (j, _) <- PM.toList v]))
  $ PM.toList
  $ mconcat
    [ PM.singleton k $ PM.singleton (moduleKey m) ()
    | ms <- mgModSummaries graph
    , let k = moduleKey $ moduleName $ ms_mod ms
    , (_, L _ m) <- ms_textual_imps ms
    , m `Set.member` ourModules
    ]
  where
    ourModules = Set.fromList $ moduleName . ms_mod <$> mgModSummaries graph