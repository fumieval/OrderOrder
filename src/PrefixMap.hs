module PrefixMap where

import Prelude hiding (lookup)
import Data.Foldable (fold)
import Data.Map.Strict qualified as M

data PrefixMap k a = Branch !(M.Map k (PrefixMap k a)) | Tip !a
  deriving (Functor, Foldable, Traversable)

instance (Ord k, Monoid a) => Semigroup (PrefixMap k a) where
  Tip a <> Tip b = Tip $ a <> b
  Tip a <> Branch m = Tip $ a <> fold (Branch m)
  Branch m <> Tip a = Tip $ fold (Branch m) <> a
  Branch m <> Branch n = Branch $ M.unionWith (\a b -> Tip $! fold a <> fold b) m n

instance (Ord k, Monoid a) => Monoid (PrefixMap k a) where
  mempty = Branch mempty

singleton :: (Ord k, Semigroup a) => [k] -> a -> PrefixMap k a
singleton [] a = Tip a
singleton (k : ks) a = Branch $ M.singleton k $ singleton ks a

toList :: PrefixMap k a -> [([k], a)]
toList (Tip a) = [([], a)]
toList (Branch m) = [(k : ks, a) | (k, v) <- M.toList m, (ks, a) <- toList v]

lookup :: (Ord k, Monoid a) => [k] -> PrefixMap k a -> Maybe a
lookup [] m = Just $! fold m
lookup (_ : _) (Tip a) = Just a
lookup (k : ks) (Branch m) = do
  v <- M.lookup k m
  lookup ks v