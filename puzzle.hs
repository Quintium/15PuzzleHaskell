import AStar
import HashTable

import Data.Array
import Data.Maybe
import Data.List
import Control.Monad

data Puzzle = Puzzle (Int, Int) (Array Int (Array Int Int)) deriving (Show, Eq)
data Move = LeftM | RightM | UpM | DownM deriving Show

moveCoords :: Move -> (Int, Int)
moveCoords LeftM = (1, 0)
moveCoords RightM = (-1, 0)
moveCoords UpM = (0, 1)
moveCoords DownM = (0, -1)

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = 0 <= x && x <= 3 && 0 <= y && y <= 3

coordsToNum :: (Int, Int) -> Int
coordsToNum (x, y) = 4 * y + x + 1

numToCoords :: Int -> (Int, Int)
numToCoords n = ((n-1) `mod` 4, (n-1) `div` 4)

solvedPuzzle :: Puzzle
solvedPuzzle = Puzzle (3, 3) $ listArray (0,3) [listArray (0,3) [coordsToNum (x, y) `mod` 16 | y <- [0..3]] | x <- [0..3]]

getMoves :: Puzzle -> [(Puzzle, Int, Move)]
getMoves puzzle@(Puzzle hole tiles) = mapMaybe (getMove puzzle) [LeftM, RightM, UpM, DownM]

getMove :: Puzzle -> Move -> Maybe (Puzzle, Int, Move)
getMove (Puzzle hole@(hx, hy) tiles) move = if inBounds newHole then Just (newPuzzle, 1, move) else Nothing
    where newPuzzle = Puzzle newHole $ switchTiles tiles hole newHole
          newHole = (hx + dx, hy + dy)
          (dx, dy) = moveCoords move

switchTiles :: Array Int (Array Int Int) -> (Int, Int) -> (Int, Int) -> Array Int (Array Int Int)
switchTiles tiles (x1, y1) (x2, y2) = writeTile (writeTile tiles (x1, y1) tile2) (x2, y2) tile1
    where tile1 = tiles ! x1 ! y1
          tile2 = tiles ! x2 ! y2

writeTile :: Array Int (Array Int Int) -> (Int, Int) -> Int -> Array Int (Array Int Int)
writeTile tiles (x, y) n = tiles // [(x, (tiles ! x) // [(y, n)])]

tileDistance :: Puzzle -> (Int, Int) -> Int
tileDistance (Puzzle _ tiles) (x, y) = if n == 0 then 0 else abs (x - solvedX) + abs(y - solvedY)
    where (solvedX, solvedY) = numToCoords n
          n = tiles ! x ! y

heuristic :: Puzzle -> Int
heuristic puzzle = sum [tileDistance puzzle (x, y) | x <- [0..3], y <- [0..3]]

instance Hashable Puzzle where
    hash :: Puzzle -> Int
    hash (Puzzle _ tiles) = sum $ map (\n -> 
        let (x, y) = numToCoords n in
            (tiles ! x ! y - 1) * (16 ^ n)) [1..16]

fromList :: [Int] -> Puzzle
fromList ns = Puzzle hole $ listArray (0,3) [listArray (0,3) [ns !! (coordsToNum (x, y) - 1) | y <- [0..3]] | x <- [0..3]]
    where hole = numToCoords (1 + fromJust (elemIndex 0 ns))

solve :: Puzzle -> Maybe (Int, [Move])
solve puzzle = aStar puzzle solvedPuzzle getMoves heuristic
