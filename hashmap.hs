module HashMap where

import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

class Eq a => Hashable a where
    hash :: a -> Int

instance Hashable Int where
    hash :: Int -> Int
    hash k = (4231 * k + 4723) `mod` 295495199

data HashMap k v = Hashable k => HashMap (IntMap [(k, v)])

emptyHM :: Hashable k => HashMap k v
emptyHM = HashMap IntMap.empty

putHM :: HashMap k v -> k -> v -> HashMap k v 
putHM (HashMap im) x y = HashMap $ IntMap.alter (updateNode x y) (hash x) im

updateNode :: Eq k => k -> v -> Maybe [(k, v)] -> Maybe [(k, v)]
updateNode x y Nothing = Just [(x, y)]
updateNode x y (Just pairs) = if not $ any ((== x) . fst) pairs then Just ((x, y):pairs) else Just $ replaceFiltered ((== x) . fst) (x, y) pairs

replaceFiltered :: (a -> Bool) -> a -> [a] -> [a]
replaceFiltered _ _ [] = []
replaceFiltered pred new (x:xs) | pred x  = new : xs
                                | otherwise = x : xs

getHM :: HashMap k v -> k -> Maybe v
getHM (HashMap im) x = do
    pairs <- IntMap.lookup (hash x) im
    snd <$> find ((== x) . fst) pairs