module AStar where

import PriorityQueue
import HashMap
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Debug.Trace

aStar :: (Eq a, Hashable a, Ord h, Num h) => a -> (a -> Bool) -> (a -> [(a, h, b)]) -> (a -> h) -> Maybe (a, h, [b])
aStar start goal edges heuristic = evalState (repeatUntilSuccess $ aStarStep goal edges heuristic) (startPQ, emptyHM)
    where startPQ = insert emptyPQ (start, 0, []) (heuristic start)

aStarStep :: (Eq a, Hashable a, Ord h, Num h) => (a -> Bool) -> (a -> [(a, h, b)]) -> (a -> h) -> State (SkewHeap (a, h, [b]) h, HashMap a Bool) (Maybe (Maybe (a, h, [b])))
aStarStep goal edges heuristic = do
    (pq, hm) <- get

    case extractMin pq of
        Nothing -> return Nothing
        Just ((minNode, dist, path), pq') -> do
            put (pq', hm)

            case getHM hm minNode of
                Just _ -> return $ Just Nothing
                Nothing -> do
                    put (pq', putHM hm minNode True)

                    if goal minNode
                    then return $ Just $ Just (minNode, dist, reverse path)
                    else do
                        forM_ (edges minNode) $ \(neighbor, len, edge) -> do
                            let newDist = dist + len
                            (prevPQ, prevHM) <- get
                            let nextPQ = insert prevPQ (neighbor, newDist, edge : path) (newDist + heuristic neighbor)
                            put (nextPQ, prevHM)                        
                        return $ Just Nothing

repeatUntilSuccess :: Monad m => m (Maybe (Maybe a)) -> m (Maybe a)
repeatUntilSuccess f = do
    res <- f
    case res of
        Nothing -> return Nothing
        Just (Just x) -> return (Just x)
        Just Nothing -> repeatUntilSuccess f

test1 :: (Hashable h, Ord h, Num h) => h -> Maybe (h, h, [String])
test1 n = aStar 0 (== n) (\i -> [(i-1, 1, "Minus1"), (i+1, 1, "Plus1")]) (\i -> abs (n - i))

edge2 :: Int -> [(Int, Int, String)]
edge2 n = [(n-1, 1, "Minus1"), (n*2, 1, "Double"), (n*3, 1, "Triple"), (n*n, 1, "Square")]

test2 :: Int -> Maybe (Int, Int, [String])
test2 n = aStar 1 (==n) edge2 (\k -> max (k - n) 0)
