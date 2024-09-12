module AStar where

import PriorityQueue
import HashMap
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Debug.Trace

repeatUntilSuccess :: Monad m => m (Maybe (Maybe a)) -> m (Maybe a)
repeatUntilSuccess f = do
    res <- f
    case res of
        Nothing -> return Nothing
        Just (Just x) -> return (Just x)
        Just Nothing -> repeatUntilSuccess f

aStar :: (Eq a, Hashable a, Ord h, Num h) => a -> (a -> Bool) -> (a -> [(a, h, b)]) -> (a -> h) -> Maybe (a, h, [b])
aStar start goal edges heuristic = evalState (repeatUntilSuccess step) (startPQ, emptyHM)
    where startPQ = insert (emptyPQ :: PriorityQueue (a, h, [b]) h) (start, 0, []) (heuristic start)
          step = do
            (pq, hm) <- get
            case extractMin pq of
                Nothing -> return Nothing
                Just ((minNode, dist, path), pq') -> do
                    put (pq', hm)
                    case getHM hm minNode of
                        Just _ -> return $ Just Nothing
                        Nothing -> do
                            let hm' = putHM hm minNode True
                            put (pq', hm')
                            if goal minNode
                            then return $ Just $ Just (minNode, dist, reverse path)
                            else do
                                forM_ (edges minNode) $ \(neighbor, len, edge) -> do
                                    let newDist = dist + len
                                    (prevPQ, prevHM) <- get
                                    let nextPQ = insert prevPQ (neighbor, newDist, edge : path) (newDist + heuristic neighbor)
                                    put (nextPQ, prevHM)                        
                                return $ Just Nothing

edge1 :: Int -> [(Int, Int, String)]
edge1 n = [(n-1, 1, "Minus1"), (n+1, 1, "Plus1")]

dist1 x y = abs (x - y)

testA n = aStar 0 (== n) edge1 (dist1 n)

edge2 :: Int -> [(Int, Int, String)]
edge2 n = [(n-1, 1, "Minus1"), (n*2, 1, "Double"), (n*3, 1, "Triple"), (n*n, 1, "Square")]

testB n = aStar 1 (==n) edge2 (\k -> max (k - n) 0)

testC n = aStar 1 (>=n) edge2 (const 0)

            
