{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Types where

import Data.Aeson
import Data.Aeson.Types
import Data.Map.Strict qualified as M
import Data.List (intercalate)
import Data.Text qualified as T

newtype ModuleName = ModuleName { getModuleName :: [String] } deriving (Eq, Ord, Show)

instance ToJSON ModuleName where
  toJSON = toJSON . render

instance ToJSONKey ModuleName where
  toJSONKey = toJSONKeyText (T.pack . render)

squash :: Int -> ModuleName -> ModuleName
squash i = ModuleName . take i . getModuleName

render :: ModuleName -> String
render = intercalate "." . getModuleName

-- | Module prefix (directory)
newtype Prefix = Prefix String deriving (Eq, Ord, Show, ToJSONKey)

-- outer key: source
-- inner key: destination
-- value: count
type Graph v = M.Map v (M.Map v Int)

toSimpleGraph :: Graph v -> M.Map v [v]
toSimpleGraph = fmap M.keys

-- | Module name without prefices
newtype Fragment = Fragment { getFragment :: String } deriving (Eq, Ord, Show, ToJSON, ToJSONKey)

-- Relationships between modules, split by prefices
type Summary = M.Map Prefix (Graph Fragment)
