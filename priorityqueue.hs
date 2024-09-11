module PriorityQueue where

import ArrayList
import Data.STRef
import Control.Monad.ST
import Control.Monad

data PriorityQueue e p = Nil | Ord p => Node (p, e) (PriorityQueue e p) (PriorityQueue e p)
newtype PriorityQueueM s e p = PriorityQueueM (STRef s (PriorityQueue e p))

(+++) :: PriorityQueue e p -> PriorityQueue e p -> PriorityQueue e p
q1 +++ Nil = q1
Nil +++ q2 = q2
q1@(Node (p1, e1) l1 r1) +++ q2@(Node (p2, e2) l2 r2) | p1 <= p2 = Node (p1, e1) (q2 +++ r1) l1
                                                      | otherwise = Node (p2, e2) (q1 +++ r2) l2

insert :: Ord p => PriorityQueue e p -> e -> p -> PriorityQueue e p
insert q x xp = q +++ Node (xp, x) Nil Nil

extractMin :: PriorityQueue e p -> Maybe (e, PriorityQueue e p)
extractMin Nil = Nothing
extractMin (Node (p1, e1) q1 q2) = Just (e1, q1 +++ q2)

emptyPQ :: ST s (PriorityQueueM s e p)
emptyPQ = PriorityQueueM <$> newSTRef Nil

insertPQ :: Ord p => PriorityQueueM s e p -> e -> p -> ST s ()
insertPQ (PriorityQueueM ref) x xp = do
    pq <- readSTRef ref
    writeSTRef ref $ insert pq x xp

extractMinPQ :: PriorityQueueM s e p -> ST s (Maybe e)
extractMinPQ (PriorityQueueM ref) = do
    pq <- readSTRef ref
    case extractMin pq of
        Nothing -> return Nothing
        Just (res, new) -> do
            writeSTRef ref new
            return $ Just res

test1 :: ST s [Maybe Int]
test1 = do
    pq :: PriorityQueueM s Int Int <- emptyPQ
    insertPQ pq 3 3
    insertPQ pq 1 1
    insertPQ pq 7 7
    insertPQ pq 2 2
    insertPQ pq 9 9
    insertPQ pq 8 8
    insertPQ pq 4 4
    insertPQ pq 6 6
    insertPQ pq 5 5

    replicateM 9 $ do
        extractMinPQ pq