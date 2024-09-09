module HashTable where

import Data.List
import Data.Maybe
import Data.Array
import Data.Array.ST
import Data.STRef
import Control.Monad.ST
import Control.Monad
import Debug.Trace

class Eq a => Hashable a where
    hash :: a -> Int

data HashTable s k v = Hashable k => HashTable Int (STArray s Int [(k, v)])
newtype HashTableM s k v = HashTableM (STRef s (HashTable s k v))

minHTCapacity :: Int
minHTCapacity = 4

htCapacity :: HashTableM s k v -> ST s Int
htCapacity (HashTableM ref) = do
    (HashTable n arr) <- readSTRef ref
    bounds <- getBounds arr
    return $ rangeSize bounds

htSize :: HashTableM s k v -> ST s Int
htSize (HashTableM ref) = do
    (HashTable n arr) <- readSTRef ref
    return n

htAssocs :: HashTableM s k v -> ST s [(k, v)]
htAssocs (HashTableM ref) = do
    (HashTable n arr) <- readSTRef ref
    es <- getElems arr
    return $ concat es

emptyHT :: Hashable k => ST s (HashTableM s k v)
emptyHT = do
    arr <- newArray (0, minHTCapacity-1) [] 
    ref <- newSTRef $ HashTable 0 arr
    return $ HashTableM ref

putHT :: HashTableM s k v -> k -> v -> ST s ()
putHT ht@(HashTableM ref) x y = do
    (HashTable n arr) <- readSTRef ref
    capacity <- htCapacity ht
    when (n >= 2 * capacity) $ do
        arr' <- newArray (0, 2*capacity-1) []
        writeSTRef ref $ HashTable n arr'
        allAssocs <- htAssocs ht
        forM_ allAssocs $ uncurry $ forcePutHT ht
    forcePutHT ht x y

forcePutHT :: HashTableM s k v -> k -> v -> ST s ()
forcePutHT ht@(HashTableM ref) x y = do
    (HashTable n arr) <- readSTRef ref
    capacity <- htCapacity ht
    let h = hash x `mod` capacity
    hAssoc <- readArray arr h
    let (f, f') = partition ((== x) . fst) hAssoc
    writeArray arr h ((x, y):f')

getHT :: HashTableM s k v -> k -> ST s (Maybe v)
getHT ht@(HashTableM ref) x = do
    (HashTable n arr) <- readSTRef ref
    capacity <- htCapacity ht
    let h = hash x `mod` capacity
    hAssoc <- readArray arr h
    let f = find ((== x) . fst) hAssoc
    return $ snd <$> f

instance Hashable Int where
    hash :: Int -> Int
    hash k = (4231 * k + 4723) `mod` 295495199

test = do
    ht ::  HashTableM s Int String <- emptyHT
    putHT ht 1 "hello"
    putHT ht 2 "this"
    putHT ht 3 "is"
    putHT ht 4 "actually"
    putHT ht 5 "cool"
    putHT ht (-1) "not"
    getHT ht (-1)
