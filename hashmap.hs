module HashMap where

import Data.List
import Data.Maybe
import Data.Array
import Data.Array.ST
import Data.STRef
import Control.Monad.ST
import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Debug.Trace
import Distribution.PackageDescription.Quirks (patchQuirks)

class Eq a => Hashable a where
    hash :: a -> Int

data HashMap s k v = Hashable k => HashMap (IntMap [(k, v)])
newtype HashMapM s k v = HashMapM (STRef s (HashMap s k v))

emptyHM :: Hashable k => ST s (HashMapM s k v)
emptyHM = do
    ref <- newSTRef $ HashMap IntMap.empty
    return $ HashMapM ref

replaceFiltered :: (a -> Bool) -> a -> [a] -> [a]
replaceFiltered _ _ [] = []
replaceFiltered pred new (x:xs) | pred x  = new : xs
                          | otherwise = x : xs

putHM :: HashMapM s k v -> k -> v -> ST s ()
putHM (HashMapM ref) x y = do
    HashMap im <- readSTRef ref
    writeSTRef ref $ HashMap $ IntMap.alter (updateNode x y) (hash x) im

updateNode :: Eq k => k -> v -> Maybe [(k, v)] -> Maybe [(k, v)]
updateNode x y Nothing = Just [(x, y)]
updateNode x y (Just pairs) = if not $ any ((== x) . fst) pairs then Just ((x, y):pairs) else Just $ replaceFiltered ((== x) . fst) (x, y) pairs

getHM :: HashMapM s k v -> k -> ST s (Maybe v)
getHM (HashMapM ref) x = do
    HashMap im <- readSTRef ref
    let pairsM = IntMap.lookup (hash x) im
    return $ do
        pairs <- pairsM
        snd <$> find ((== x) . fst) pairs

instance Hashable Int where
    hash :: Int -> Int
    hash k = (4231 * k + 4723) `mod` 295495199