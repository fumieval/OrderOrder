{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import Control.Monad
import Data.ByteString.Char8 qualified as B
import Data.Yaml qualified as Yaml
import Options.Applicative qualified as O

import Parse
import Graph

main :: IO ()
main = join $ O.execParser $ flip O.info mempty $ do
  dumpSummary <- O.switch $ O.long "summary" <> O.help "Dump summarised dependencies"
  dumpDot <- O.switch $ O.long "dot" <> O.help "Dump dot representation of the summary"
  dumpFAS <- O.switch $ O.long "fas" <> O.help "Dump a feedback arc set"
  squashBelow <- O.option O.auto $ O.long "squash-below" <> O.metavar "N" <> O.help "collapse nodes deeper than N" <> O.value maxBound
  sourceDirs <- O.some $ O.strArgument $ O.metavar "DIR"
  _ <- O.helper
  pure $ do
    graph <- fmap mconcat $ forM sourceDirs $ \dir -> do
      srcs <- enumSources dir
      mconcat <$> traverse (getEntry dir) srcs
    let pruned = prune graph
    let summary = summarise squashBelow pruned
    when dumpSummary $ B.putStrLn $ Yaml.encode summary
    when dumpDot $ putStrLn $ toDot summary
    when dumpFAS $ putStr $ unlines $ suggestTrims pruned $ findFAS <$> summary
