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

data OutputFormat = Raw | Summary | Dot | FAS

parseOutputFormat :: String -> Maybe OutputFormat
parseOutputFormat = \case
  "raw" -> Just Raw
  "summary" -> Just Summary
  "dot" -> Just Dot
  "fas" -> Just FAS
  _ -> Nothing

main :: IO ()
main = join $ O.execParser $ flip O.info mempty $ do
  format <- O.option (O.maybeReader parseOutputFormat) $ O.long "format" <> O.value FAS
  squashBelow <- O.option O.auto $ O.long "squash-below" <> O.metavar "N" <> O.help "collapse nodes deeper than N" <> O.value maxBound
  sourceDirs <- O.some $ O.strArgument $ O.metavar "DIR"
  _ <- O.helper
  pure $ do
    graph <- fmap mconcat $ forM sourceDirs $ \dir -> do
      srcs <- enumSources dir
      mconcat <$> traverse (getEntry dir) srcs
    let pruned = prune graph
    let summary = summarise squashBelow pruned
    case format of
      Raw -> B.putStrLn $ Yaml.encode pruned
      Summary -> B.putStrLn $ Yaml.encode summary
      Dot -> putStrLn $ toDot summary
      FAS -> putStr $ unlines $ suggestTrims pruned $ findFAS <$> summary
