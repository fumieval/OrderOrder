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
import Text.Dot qualified as Dot

import Parse
import Graph
import Types

data OutputFormat = Yaml | Dot

data Process = Raw | Summary | FAS

parseOutputFormat :: String -> Maybe OutputFormat
parseOutputFormat = \case
  "yaml" -> Just Yaml
  "dot" -> Just Dot
  _ -> Nothing

parseProcess :: String -> Maybe Process
parseProcess = \case
  "raw" -> Just Raw
  "summary" -> Just Summary
  "fas" -> Just FAS
  _ -> Nothing

main :: IO ()
main = join $ O.execParser $ flip O.info mempty $ do
  format <- O.option (O.maybeReader parseOutputFormat) $ O.long "format" <> O.value Yaml
  squashBelow <- O.option O.auto $ O.long "squash-below" <> O.metavar "N" <> O.help "collapse nodes deeper than N" <> O.value maxBound
  sourceDirs <- O.some $ O.strArgument $ O.metavar "DIR"
  process <- O.option (O.maybeReader parseProcess) $ O.long "process" <> O.value FAS
  _ <- O.helper
  pure $ do
    graph <- fmap mconcat $ forM sourceDirs $ \dir -> do
      srcs <- enumSources dir
      mconcat <$> traverse (getEntry dir) srcs
    let pruned = prune graph
    let summary = summarise squashBelow pruned
    case process of
      Raw -> case format of
        Yaml -> B.putStrLn $ Yaml.encode $ toSimpleGraph pruned
        Dot -> putStrLn $ showDot' $ cluster render pruned
      Summary -> case format of
        Yaml -> B.putStrLn $ Yaml.encode $ toSimpleGraph <$> summary
        Dot -> putStrLn $ showDot' $ summaryToDot summary
      FAS -> do
        let fas = suggestTrims pruned $ findFAS <$> summary
        case format of
          Yaml -> B.putStrLn $ Yaml.encode $ toSimpleGraph fas
          Dot -> putStrLn $ showDot' $ cluster render fas
