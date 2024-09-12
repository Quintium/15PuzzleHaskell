import AStar
import HashMap

import Data.Array
import Data.Maybe
import Data.List
import Debug.Trace

data Puzzle = Puzzle (Int, Int) (Array Int Int) Int Int deriving Eq
data Move = LeftM | RightM | UpM | DownM deriving Show

instance Show Puzzle where
    show :: Puzzle -> String
    show (Puzzle _ tiles _ _) = show $ elems tiles

-- how the hole moves during moves
moveCoords :: Move -> (Int, Int)
moveCoords LeftM = (1, 0)
moveCoords RightM = (-1, 0)
moveCoords UpM = (0, 1)
moveCoords DownM = (0, -1)

moveChar :: Move -> Char
moveChar LeftM = 'L'
moveChar RightM = 'R'
moveChar UpM = 'U'
moveChar DownM = 'D'

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = 0 <= x && x <= 3 && 0 <= y && y <= 3

coordsToInd :: (Int, Int) -> Int
coordsToInd (x, y) = 4 * y + x

indToCoords :: Int -> (Int, Int)
indToCoords n = (n `mod` 4, n `div` 4)

manualHash :: Array Int Int -> Int
manualHash tiles = sum $ map (\(i, n) -> n * (16 ^ i)) $ assocs tiles

manualHeuristic :: Array Int Int -> Int
manualHeuristic tiles = sum $ map (uncurry tileDistance) $ assocs tiles

instance Hashable Puzzle where
    hash :: Puzzle -> Int
    hash (Puzzle _ _ h _) = h

heuristic :: Puzzle -> Int
heuristic (Puzzle _ _ _ h) = h

fromList :: [Int] -> Puzzle
fromList ns = Puzzle hole tiles (manualHash tiles) (manualHeuristic tiles)
    where tiles = listArray (0,15) ns
          hole = indToCoords $ fromJust $ elemIndex 0 ns

solvedPuzzle :: Puzzle
solvedPuzzle = fromList ([1..15] ++ [0])

getMoves :: Puzzle -> [(Puzzle, Int, Move)]
getMoves puzzle = mapMaybe (getMove puzzle) [LeftM, RightM, UpM, DownM]

getMove :: Puzzle -> Move -> Maybe (Puzzle, Int, Move)
getMove (Puzzle hole@(hx, hy) tiles hash heur) move = if inBounds newHole then Just (newPuzzle, 1, move) else Nothing
    where newPuzzle = Puzzle newHole (switchTiles tiles hole newHole) newHash newHeur
          newHash = hash + movedTile * (16 ^ holeInd - 16 ^ newHoleInd)
          newHeur = heur + tileDistance holeInd movedTile - tileDistance newHoleInd movedTile
          movedTile = tiles ! newHoleInd
          holeInd = coordsToInd hole
          newHoleInd = coordsToInd newHole
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

optimalSolve :: Puzzle -> Maybe (Int, String)
optimalSolve puzzle = (\(p, n, ms) -> (n, map moveChar ms)) <$> aStar puzzle (== solvedPuzzle) getMoves heuristic

solveInParts :: [Int] -> Puzzle -> Maybe (Int, String)
solveInParts steps puzzle = do
    (_, ms) <- foldl solveOnePart (Just (puzzle, [])) steps
    return (length ms, map moveChar ms)

solveInPartsWithPuzzle :: [Int] -> Puzzle -> Maybe (Puzzle, Int, String)
solveInPartsWithPuzzle steps puzzle = do
    (p, ms) <- foldl solveOnePart (Just (puzzle, [])) steps
    return (p, length ms, map moveChar ms)

suboptimalSolve :: Puzzle -> Maybe (Int, String)
suboptimalSolve = solveInParts [10,0]

solveOnePart :: Maybe (Puzzle, [Move]) -> Int -> Maybe (Puzzle, [Move])
solveOnePart accumMaybe goal = do
    (puzzle, ms) <- accumMaybe
    (p', _, ms') <- aStar puzzle (\p -> heuristic p <= goal) getMoves (\p -> max (heuristic p - goal) 0)
    return $ trace ("Goal achieved: " ++ show goal) (p', ms ++ ms')

main :: IO ()
main = do
    print $ solveInParts [10,0] $ fromList [4,0,3,14,2,15,10,11,8,5,6,12,7,13,1,9]