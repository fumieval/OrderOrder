module Graph where
  
import Control.Monad
import Data.Foldable
import Data.List (sortBy)
import Data.Map.Strict qualified as M
import Data.Ord (comparing)
import Data.Set qualified as Set
import Text.Dot qualified as Dot
import Types

-- drop external modules
prune :: Ord k => Graph k -> Graph k
prune graph = M.filterWithKey (\k _ -> M.member k graph) <$> graph

relations :: ModuleName -> ModuleName -> [(Fragment, Fragment, Prefix)]
relations (ModuleName xs0) (ModuleName ys0) = go xs0 ys0 where
  go [] _ = []
  go _ [] = []
  go (x : xs) (y : ys)
    | x == y = fmap (prefix x) <$> go xs ys
    | otherwise = [(Fragment x, Fragment y, Prefix "")]
  prefix x (Prefix "") = Prefix x
  prefix x (Prefix y) = Prefix (x <> "." <> y)

summarise :: Int -> Graph ModuleName -> Summary
summarise squashLevel graph = M.fromListWith (M.unionWith (M.unionWith (+)))
  [ el
  | (src, dsts) <- M.toList graph
  , (dst, weight) <- M.toList dsts
  , (k, d, prefix) <- relations (squash squashLevel src) (squash squashLevel dst)
  , el <- [(prefix, M.singleton k $ M.singleton d weight), (prefix, M.singleton d mempty)]
  ]

-- TODO: better data structure
type Connectivity a = M.Map a (Set.Set a)

isConnected :: Ord a => Connectivity a -> a -> a -> Bool
isConnected conn src dst = case M.lookup src conn of
  Just set -> Set.member dst set || any (\x -> isConnected conn x dst) set
  Nothing -> False

insertConn :: Ord a => a -> a -> Connectivity a -> Connectivity a
insertConn src dst = M.insertWith (<>) src (Set.singleton dst)

-- | Find a feedback arc set (a set of edges that would eliminate SCCs from the given graph)
findFAS :: Ord k => Graph k -> Graph k
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
suggestTrims :: Graph ModuleName -> M.Map Prefix (Graph Fragment) -> Graph ModuleName
suggestTrims rawGraph trimMap = M.fromList
  [ (src, M.singleton dst 1)
  | (src, dsts) <- M.toList rawGraph
  , dst <- M.keys dsts
  , (k, d, prefix) <- relations src dst
  , trims <- toList $ M.lookup prefix trimMap
  , ds <- toList $ M.lookup k trims
  , M.member d ds
  ]

cluster :: Ord a => (a -> String) -> Graph a -> Dot.Dot ()
cluster key group = do
  nodes <- sequence $ M.fromList
    [ (k, Dot.node [("label", key k)])
    | k <- M.keys group ++ foldMap M.keys (M.elems group)
    ]
  forM_ (M.toList group) $ \(k, vs) -> do
    forM_ (M.toList vs) $ \(v, weight) -> forM_ (M.lookup v nodes)
      $ \n' -> Dot.edge (nodes M.! k) n' [("weight", show weight), ("label", show weight)]

summaryToDot :: Summary -> Dot.Dot ()
summaryToDot summary = forM_ (M.toList summary) $ \(Prefix prefix, group) -> Dot.cluster $ do
  Dot.attribute ("label", prefix)
  cluster getFragment group

showDot' :: Dot.Dot () -> String
showDot' dot = Dot.showDot $ do
  Dot.attribute ("rankdir", "LR")
  dot