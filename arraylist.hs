module ArrayList where

import Data.List
import Data.Maybe
import Data.Array
import Data.Array.ST
import Data.STRef
import Control.Monad.ST
import Control.Monad
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

emptyAL :: ST s (ArrayListM s e)
emptyAL = do
    arr <- newArray_ (0, minALCapacity-1) 
    ref <- newSTRef $ ArrayList 0 arr
    return $ ArrayListM ref

pushAL :: ArrayListM s e -> e -> ST s ()
pushAL al@(ArrayListM ref) x = do
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

popAL :: ArrayListM s e -> ST s ()
popAL al@(ArrayListM ref) = do
    (ArrayList n arr) <- readSTRef ref

    when (n /= 0) $ do
        capacity <- alCapacity al
        if 4*(n-1) == capacity && n-1 >= minALCapacity then
            (do
                es <- getElems arr
                arr' <- newListArray (0, 2*(n-1)-1) $ init es
                writeSTRef ref $ ArrayList (n-1) arr'
            )
        else
            writeSTRef ref $ ArrayList (n-1) arr

readAL :: ArrayListM s e -> Int -> ST s e
readAL (ArrayListM ref) i = do
    (ArrayList n arr) <- readSTRef ref
    readArray arr i

writeAL :: ArrayListM s e -> Int -> e -> ST s ()
writeAL (ArrayListM ref) i x = do
    (ArrayList n arr) <- readSTRef ref
    writeArray arr i x

test = do
    al <- emptyAL
    pushAL al 1
    pushAL al 2
    pushAL al 3
    pushAL al 4
    popAL al
    pushAL al 5
    popAL al
    pushAL al 6
    alToList al
