module PriorityQueue where

import Control.Monad.ST

class Ord b => PriorityQueue q a b where
    empty :: q a b
    insert :: q a b -> a -> b -> ST s ()
    extractMin :: q a b -> ST s (a, b)
    --decreaseWeight :: 
