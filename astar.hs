module AStar where

import PriorityQueue
import HashMap
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Debug.Trace

-- general A* algorithm
-- a: vertex type, b: edge name type, h: heuristic type
-- start     = start vertex
-- goal      = predicate of the vertices to be searched
-- edges     = function from a vertex to a list of all connected edges in the form of (edgeName, edgeLength, neighbor)
-- heuristic = function from vertex to lower bound of distance to goal vertices, used to guide the search
aStar :: (Eq a, Hashable a, Ord h, Num h) => a -> (a -> Bool) -> (a -> [(b, h, a)]) -> (a -> h) -> Maybe (a, h, [b])
aStar start goal edges heuristic = evalState (repeatUntilSuccess $ aStarStep goal edges heuristic) (startPQ, emptyHM)
    where startPQ = insert emptyPQ (start, 0, []) (heuristic start)

-- state: skew heap and hash map
-- step returns (Just res) if search ended, Nothing if search has to be continued; res is in form of Maybe (goalNode, length, edges)
-- skew heap (priority queue) tracks all vertices to be examined, as well as their minimum potential total distance to the goal and the moves to get there
-- hash map tracks all vertices already examined, to be skipped when examined again
aStarStep :: (Eq a, Hashable a, Ord h, Num h) => (a -> Bool) -> (a -> [(b, h, a)]) -> (a -> h) -> State (SkewHeap (a, h, [b]) h, HashMap a Bool) (Maybe (Maybe (a, h, [b])))
aStarStep goal edges heuristic = do
    (pq, hm) <- get

    -- pick most promising vertex
    case extractMin pq of
        Nothing -> return $ Just Nothing
        Just ((minNode, dist, path), pq') -> do
            put (pq', hm)

            -- check whether vertex was already examined
            case getHM hm minNode of
                Just _ -> return Nothing
                Nothing -> do
                    -- save that vertex is being examined
                    put (pq', putHM hm minNode True)

                    if goal minNode
                    then return $ Just $ Just (minNode, dist, reverse path)
                    else do
                        -- store all neighbors for examination
                        forM_ (edges minNode) $ \(edge, len, neighbor) -> do
                            let newDist = dist + len
                            (prevPQ, prevHM) <- get
                            let nextPQ = insert prevPQ (neighbor, newDist, edge : path) (newDist + heuristic neighbor)
                            put (nextPQ, prevHM)                        
                        return Nothing

-- repeat monad action until it returns (Just res)
repeatUntilSuccess :: Monad m => m (Maybe a) -> m a
repeatUntilSuccess f = do
    res <- f
    case res of
        Just res -> return res
        Nothing -> repeatUntilSuccess f

-- simple A* test - node to previous and next integer, has to find n from 0, easy using heuristic
test1 :: (Hashable h, Ord h, Num h) => h -> Maybe (h, h, [String])
test1 n = aStar 0 (== n) (\i -> [("Minus1", 1, i-1), ("Plus1", 1, i+1)]) (\i -> abs (n - i))

-- more interesting A* test, has to find integer from 1, by subtracting 1, doubling, tripling or squaring
test2 :: Int -> Maybe (Int, Int, [String])
test2 n = aStar 1 (==n) edge2 (\k -> max (k - n) 0)

edge2 :: Int -> [(String, Int, Int)]
edge2 n = [("Minus1", 1, n-1), ("Double", 1, n*2), ("Triple", 1, n*3), ("Square", 1, n*n)]
