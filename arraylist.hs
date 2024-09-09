module ArrayList where

import Data.List
import Data.Maybe
import Data.Array
import Data.Array.ST
import Data.STRef
import Control.Monad.ST
import Debug.Trace

data ArrayList s e = ArrayList Int (STArray s Int e)
newtype ArrayListM s e = ArrayListM (STRef s (ArrayList s e))

minALCapacity :: Int
minALCapacity = 4

alCapacity :: ArrayListM s e -> ST s Int
alCapacity (ArrayListM ref) = do
    (ArrayList n arr) <- readSTRef ref
    bounds <- getBounds arr
    return $ rangeSize bounds

alLength :: ArrayListM s e -> ST s Int
alLength (ArrayListM ref) = do
    (ArrayList n arr) <- readSTRef ref
    return n

alToArray :: ArrayListM s e -> ST s (Array Int e)
alToArray (ArrayListM ref) = do
    (ArrayList n arr) <- readSTRef ref
    es <- getElems arr
    return $ listArray (0,n-1) es

alToList :: ArrayListM s e -> ST s [e]
alToList al = elems <$> alToArray al

newAL :: ST s (ArrayListM s e)
newAL = do
    arr <- newArray_ (0, minALCapacity-1) 
    ref <- newSTRef $ ArrayList 0 arr
    return $ ArrayListM ref

push :: ArrayListM s e -> e -> ST s ()
push al@(ArrayListM ref) x = do
    (ArrayList n arr) <- readSTRef ref
    capacity <- alCapacity al
    if n == capacity then
        (do
            es <- getElems arr
            arr' <- newListArray (0,2*n-1) (es ++ [x])
            writeSTRef ref $ ArrayList (n+1) arr'
        )
    else
        (do
            writeArray arr n x
            writeSTRef ref $ ArrayList (n+1) arr
        )

pop :: ArrayListM s e -> ST s ()
pop al@(ArrayListM ref) = do
    (ArrayList n arr) <- readSTRef ref

    capacity <- alCapacity al
    if 4*(n-1) == capacity && n-1 >= minALCapacity then
        (do
            es <- getElems arr
            arr' <- newListArray (0, 2*(n-1)-1) $ init es
            writeSTRef ref $ ArrayList (n-1) arr'
        )
    else
        writeSTRef ref $ ArrayList (n-1) arr

readAt :: ArrayListM s e -> Int -> ST s e
readAt (ArrayListM ref) i = do
    (ArrayList n arr) <- readSTRef ref
    readArray arr i

writeAt :: ArrayListM s e -> Int -> e -> ST s ()
writeAt (ArrayListM ref) i x = do
    (ArrayList n arr) <- readSTRef ref
    writeArray arr i x

test = do
    al <- newAL
    push al 1
    push al 2
    push al 3
    push al 4
    pop al
    push al 5
    pop al
    push al 6
    alToList al
