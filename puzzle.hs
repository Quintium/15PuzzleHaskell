import AStar
import HashTable

import Data.Array
import Data.Maybe
import Data.List
import Debug.Trace

data Puzzle = Puzzle (Int, Int) (Array Int Int) deriving Eq
data Move = LeftM | RightM | UpM | DownM deriving Show

instance Show Puzzle where
    show :: Puzzle -> String
    show (Puzzle _ tiles) = show $ elems tiles

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

fromList :: [Int] -> Puzzle
fromList ns = Puzzle hole $ listArray (0,15) ns
    where hole = indToCoords $ fromJust $ elemIndex 0 ns

solvedPuzzle :: Puzzle
solvedPuzzle = fromList ([1..15] ++ [0])

getMoves :: Puzzle -> [(Puzzle, Float, Move)]
getMoves puzzle = mapMaybe (getMove puzzle) [LeftM, RightM, UpM, DownM]

getMove :: Puzzle -> Move -> Maybe (Puzzle, Float, Move)
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

tileDistance :: Int -> Int -> Int
tileDistance i n = if n == 0 then 0 else abs (x - solvedX) + abs(y - solvedY)
    where (solvedX, solvedY) = indToCoords (n-1)
          (x, y) = indToCoords i

heuristic :: Puzzle -> Int
heuristic (Puzzle _ tiles) = sum $ map (uncurry tileDistance) $ assocs tiles

inversionsH :: Puzzle -> Float
inversionsH puzzle = (sum [if inversion puzzle i j then 1 else 0 | i <- [0..15], j <- [i+1..15]]) / 3

inversion :: Puzzle -> Int -> Int -> Bool
inversion _ 0 _ = False
inversion _ _ 0 = False
inversion (Puzzle _ tiles) i j = tiles ! i > tiles ! j

instance Hashable Puzzle where
    hash :: Puzzle -> Int
    hash (Puzzle _ tiles) = sum $ map (\(i, n) -> n * (16 ^ i)) $ assocs tiles

solve :: Puzzle -> Maybe (Float, [Move])
solve puzzle = (\(p, n, ms) -> (n, ms)) <$> aStar puzzle (== solvedPuzzle) getMoves inversionsH

partSolve :: Float -> Puzzle -> Maybe (Puzzle, Float, [Move])
partSolve n puzzle = aStar puzzle (\p -> inversionsH p <= n) getMoves (\p -> max (inversionsH p - n) 0)

solveInParts :: [Float] -> Puzzle -> Maybe (Puzzle, Int, [Move])
solveInParts steps puzzle = (\(p, ms) -> (p, length ms, ms)) <$> foldl solveOnePart (Just (puzzle, [])) steps

solveOnePart :: Maybe (Puzzle, [Move]) -> Float -> Maybe (Puzzle, [Move])
solveOnePart accumMaybe goal = do
    (p, ms) <- accumMaybe
    (p', _, ms') <- partSolve goal p
    return $ trace (show goal) (p', ms ++ ms')

solveSuboptimal :: Puzzle -> Maybe (Int, String)
solveSuboptimal puzzle = (\(p, n, ms) -> (n, map moveChar ms)) <$> solveInParts [20,10,7,5,3,0] puzzle

moveChar :: Move -> Char
moveChar LeftM = 'L'
moveChar RightM = 'R'
moveChar UpM = 'U'
moveChar DownM = 'D'

main :: IO ()
main = do
    print $ solve $ fromList [1,2,3,4,5,6,7,8,9,10,11,12,15,13,14,0]