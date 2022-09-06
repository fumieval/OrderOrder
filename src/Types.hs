{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Types where

import Data.Aeson (ToJSONKey(..))
import Data.Map.Strict qualified as M
import Data.List (intercalate)

newtype ModuleName = ModuleName { getModuleName :: [String] } deriving (Eq, Ord, Show, ToJSONKey)

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

-- | Module name without prefices
newtype Fragment = Fragment String deriving (Eq, Ord, Show, ToJSONKey)

-- Relationships between modules, split by prefices
type Summary = M.Map Prefix (Graph Fragment)
