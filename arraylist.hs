import Data.List
import Data.Maybe
import Data.Array
import Data.Array.ST
import Data.STRef
import Control.Monad.ST
import Debug.Trace

data ArrayList s e = ArrayList Int (STArray s Int e)
type ArrayListM s e = STRef s (ArrayList s e)

minALBound :: Int
minALBound = 4

alBound :: ArrayListM s e -> ST s Int
alBound ref = do
    (ArrayList n arr) <- readSTRef ref
    bounds <- getBounds arr
    return $ rangeSize bounds

alLength :: ArrayListM s e -> ST s Int
alLength ref = do
    (ArrayList n arr) <- readSTRef ref
    return n

alToArray :: ArrayListM s e -> ST s (Array Int e)
alToArray ref = do
    (ArrayList n arr) <- readSTRef ref
    es <- getElems arr
    return $ listArray (0,n-1) es

alToList :: ArrayListM s e -> ST s [e]
alToList ref = elems <$> alToArray ref

newAL :: ST s (ArrayListM s e)
newAL = do
    arr <- newArray_ (0, minALBound-1) 
    let al = ArrayList 0 arr
    newSTRef al

push :: ArrayListM s e -> e -> ST s ()
push ref x = do
    (ArrayList n arr) <- readSTRef ref
    bound <- alBound ref
    if n == bound then
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
pop ref = do
    (ArrayList n arr) <- readSTRef ref

    bound <- alBound ref
    if 4*(n-1) == bound && n-1 >= minALBound then
        (do
            es <- getElems arr
            arr' <- newListArray (0, 2*(n-1)-1) $ init es
            writeSTRef ref $ ArrayList (n-1) arr'
        )
    else
        writeSTRef ref $ ArrayList (n-1) arr

readAt :: ArrayListM s e -> Int -> ST s e
readAt ref i = do
    (ArrayList n arr) <- readSTRef ref
    readArray arr i

writeAt :: ArrayListM s e -> Int -> e -> ST s ()
writeAt ref i x = do
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