import AStar
import HashTable

import Data.Array
import Data.Maybe
import Data.List

data Puzzle = Puzzle (Int, Int) (Array Int Int) deriving (Show, Eq)
data Move = LeftM | RightM | UpM | DownM deriving Show

-- how the hole moves during moves
moveCoords :: Move -> (Int, Int)
moveCoords LeftM = (1, 0)
moveCoords RightM = (-1, 0)
moveCoords UpM = (0, 1)
moveCoords DownM = (0, -1)

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = 0 <= x && x <= 3 && 0 <= y && y <= 3

coordsToInd :: (Int, Int) -> Int
coordsToInd (x, y) = 4 * y + x

indToCoords :: Int -> (Int, Int)
indToCoords n = (n `mod` 4, n `div` 4)

solvedPuzzle :: Puzzle
solvedPuzzle = Puzzle (3, 3) $ listArray (0,15) $ [1..15] ++ [0]

getMoves :: Puzzle -> [(Puzzle, Int, Move)]
getMoves puzzle = mapMaybe (getMove puzzle) [LeftM, RightM, UpM, DownM]

getMove :: Puzzle -> Move -> Maybe (Puzzle, Int, Move)
getMove (Puzzle hole@(hx, hy) tiles) move = if inBounds newHole then Just (newPuzzle, 1, move) else Nothing
    where newPuzzle = Puzzle newHole $ switchTiles tiles hole newHole
          newHole = (hx + dx, hy + dy)
          (dx, dy) = moveCoords move

switchTiles :: Array Int Int -> (Int, Int) -> (Int, Int) -> Array Int Int
switchTiles tiles c1 c2 = tiles // [(i1, n2), (i2, n1)]
    where i1 = coordsToInd c1
          i2 = coordsToInd c2
          n1 = tiles ! i1
          n2 = tiles ! i2

writeTile :: Array Int (Array Int Int) -> (Int, Int) -> Int -> Array Int (Array Int Int)
writeTile tiles (x, y) n = tiles // [(x, (tiles ! x) // [(y, n)])]

tileDistance :: Puzzle -> (Int, Int) -> Int
tileDistance (Puzzle _ tiles) (x, y) = if n == 0 then 0 else abs (x - solvedX) + abs(y - solvedY)
    where (solvedX, solvedY) = indToCoords (n-1)
          n = tiles ! coordsToInd (x, y)

heuristic :: Puzzle -> Int
heuristic puzzle = sum [tileDistance puzzle (x, y) | x <- [0..3], y <- [0..3]]

instance Hashable Puzzle where
    hash :: Puzzle -> Int
    hash (Puzzle _ tiles) = sum $ map (\i -> (tiles ! i) * (16 ^ i)) [0..15]

fromList :: [Int] -> Puzzle
fromList ns = Puzzle hole $ listArray (0,15) ns
    where hole = indToCoords $ fromJust $ elemIndex 0 ns

solve :: Puzzle -> Maybe (Int, [Move])
solve puzzle = aStar puzzle solvedPuzzle getMoves heuristic

main :: IO ()
main = do
    print $ solve $ fromList [1,2,3,4,5,6,7,8,9,10,11,12,15,13,14,0]