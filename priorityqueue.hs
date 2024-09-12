module PriorityQueue where

data PriorityQueue e p = Empty | Ord p => Node (p, e) (PriorityQueue e p) (PriorityQueue e p)

emptyPQ :: PriorityQueue e p
emptyPQ = Empty

(+++) :: PriorityQueue e p -> PriorityQueue e p -> PriorityQueue e p
q1 +++ Empty = q1
Empty +++ q2 = q2
q1@(Node (p1, e1) l1 r1) +++ q2@(Node (p2, e2) l2 r2) | p1 <= p2 = Node (p1, e1) (q2 +++ r1) l1
                                                      | otherwise = Node (p2, e2) (q1 +++ r2) l2

insert :: Ord p => PriorityQueue e p -> e -> p -> PriorityQueue e p
insert q x xp = q +++ Node (xp, x) Empty Empty

extractMin :: PriorityQueue e p -> Maybe (e, PriorityQueue e p)
extractMin Empty = Nothing
extractMin (Node (p1, e1) q1 q2) = Just (e1, q1 +++ q2)