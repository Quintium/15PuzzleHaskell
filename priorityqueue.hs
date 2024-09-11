module PriorityQueue where

import ArrayList
import Data.STRef
import Control.Monad.ST
import Control.Monad

data PriorityQueueM s e p = Ord p => PriorityQueueM (ArrayListM s (p, e))

emptyPQ :: Ord p => ST s (PriorityQueueM s e p)
emptyPQ = PriorityQueueM <$> emptyAL

insert :: PriorityQueueM s e p -> e -> p -> ST s ()
insert pq@(PriorityQueueM al) x k = do
    pushAL al (k, x)
    len <- alLength al
    bubbleUp pq (len-1)

switch :: PriorityQueueM s e p -> Int -> Int -> ST s ()
switch pq@(PriorityQueueM al) i j = do
    (k1, x1) <- readAL al i
    (k2, x2) <- readAL al j
    writeAL al i (k2, x2)
    writeAL al j (k1, x1)

bubbleUp :: PriorityQueueM s e p -> Int -> ST s ()
bubbleUp pq@(PriorityQueueM al) i = do
    when (i /= 0) $ do
        let j = (i-1) `div` 2
        (k1, curr) <- readAL al i
        (k2, parent) <- readAL al j
        when (k1 < k2) $ do
            switch pq i j
            bubbleUp pq j

extractMin :: PriorityQueueM s e p -> ST s (Maybe e)
extractMin pq@(PriorityQueueM al) = do
    len <- alLength al
    if len /= 0
    then do
        len <- alLength al
        switch pq 0 (len-1)

        (_, min) <- readAL al (len-1)
        popAL al

        bubbleDown pq 0
        return $ Just min
    else return Nothing

bubbleDown :: PriorityQueueM s e p -> Int -> ST s ()
bubbleDown pq@(PriorityQueueM al) i = do
    let j1 = 2*i + 1
    let j2 = 2*i + 2
    len <- alLength al

    when (j1 < len) $ do
        (k, curr) <- readAL al i
        (l1, _) <- readAL al j1

        if j2 < len 
        then do
            (l2, _) <- readAL al j2
            when (l1 < k || l2 < k) $ do
                if l1 < l2 
                then do
                    switch pq i j1
                    bubbleDown pq j1
                else do
                    switch pq i j2
                    bubbleDown pq j2
        else do
            when (l1 < k) $ do
                switch pq i j1
                bubbleDown pq j1

heap :: PriorityQueueM s e p -> ST s [(p, e)]
heap (PriorityQueueM al) = alToList al

test1 :: ST s [Maybe Int]
test1 = do
    pq :: PriorityQueueM s Int Int <- emptyPQ
    insert pq 3 3
    insert pq 1 1
    insert pq 7 7
    insert pq 2 2
    insert pq 9 9
    insert pq 8 8
    insert pq 4 4
    insert pq 6 6
    insert pq 5 5

    replicateM 9 $ do
        extractMin pq