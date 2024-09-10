module AStar where

import PriorityQueue
import HashTable
import Data.STRef
import Data.Maybe
import Control.Monad.ST
import Control.Monad
import Debug.Trace

repeatUntil :: Monad m => m (Maybe Bool) -> m Bool
repeatUntil f = do
    res <- f
    case res of
        Nothing -> return False
        Just True -> return True
        Just False -> repeatUntil f

aStar :: (Eq a, Hashable a, Ord h, Num h) => a -> a -> (a -> [(a, h, b)]) -> (a -> h) -> Maybe (h, [b])
aStar start end edges heuristic = runST $ do
    pq :: PriorityQueueM s a h <- emptyPQ
    ht :: HashTableM s a (h, [b]) <- emptyHT

    decreaseWeight pq start (heuristic start)
    putHT ht start (0, [])

    success <- repeatUntil (step pq ht)
    if success
    then do
        entry <- getHT ht end
        let (dist, path) = fromJust entry
        return $ Just (dist, reverse path)
    else
        return Nothing

    where step pq ht = do
            minMaybe <- extractMin pq
            case minMaybe of
                (Just min) -> do
                    if min == end
                    then return $ Just True
                    else do
                        entry <- getHT ht min
                        let (dist, path) = fromJust entry
                        forM_ (edges min) $ \(vertice, len, edge) -> do
                            let newDist = dist + len
                            statsMaybe <- getHT ht vertice
                            let better = maybe True (\(currDist, _) -> newDist < currDist) statsMaybe
                            when better $ do
                                decreaseWeight pq vertice (newDist + heuristic vertice)
                                putHT ht vertice (newDist, edge : path)
                            
                        return $ Just False
                Nothing -> return Nothing

edge1 :: Int -> [(Int, Int, String)]
edge1 n = [(n-1, 1, "Minus1"), (n+1, 1, "Plus1")]

dist1 x y = abs (x - y)

testA n = aStar 0 n edge1 (dist1 n)

edge2 :: Int -> [(Int, Int, String)]
edge2 n = [(n-1, 1, "Minus1"), (n*2, 1, "Double"), (n*3, 1, "Triple"), (n*n, 1, "Square")]

testB n = aStar 1 n edge2 (\k -> max (k - n) 0)

            
