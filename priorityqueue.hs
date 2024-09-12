module PriorityQueue where

-- Implements priority queue using skew heap
-- Implementation is extremely simple but guarantees amortized runtime of O(log n) through strange union function
data SkewHeap e p = Empty | Ord p => Node (p, e) (SkewHeap e p) (SkewHeap e p)

emptyPQ :: SkewHeap e p
emptyPQ = Empty

-- union of two skew heaps
(+++) :: SkewHeap e p -> SkewHeap e p -> SkewHeap e p
h1 +++ Empty = h1
Empty +++ h2 = h2
-- flipping of left and right subtree is essential for runtime
h1@(Node (p1, e1) l1 r1) +++ h2@(Node (p2, e2) l2 r2) | p1 <= p2 = Node (p1, e1) (h2 +++ r1) l1
                                                      | otherwise = Node (p2, e2) (h1 +++ r2) l2

insert :: Ord p => SkewHeap e p -> e -> p -> SkewHeap e p
insert h x xp = h +++ Node (xp, x) Empty Empty

extractMin :: SkewHeap e p -> Maybe (e, SkewHeap e p)
extractMin Empty = Nothing
extractMin (Node (p1, e1) h1 h2) = Just (e1, h1 +++ h2)