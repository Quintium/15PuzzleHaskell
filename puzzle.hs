import AStar
import HashMap

import Data.Array
import Data.Maybe
import Data.List
import Debug.Trace

-- 15 Puzzle - Puzzle holeIndex tiles hash heuristic
data Puzzle = Puzzle Int (Array Int Int) Int Int deriving Eq
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

-- convert tile coordinates to tile array index and vice versa
coordsToInd :: (Int, Int) -> Int
coordsToInd (x, y) = 4 * y + x

indToCoords :: Int -> (Int, Int)
indToCoords n = (n `mod` 4, n `div` 4)

-- simple polynomial hash
manualHash :: Array Int Int -> Int
manualHash tiles = sum $ map (\(i, n) -> n * (16 ^ i)) $ assocs tiles

-- sum of manhattan distances of tiles (except hole)
manualHeuristic :: Array Int Int -> Int
manualHeuristic tiles = sum $ map (uncurry tileDistance) $ assocs tiles

tileDistance :: Int -> Int -> Int
tileDistance i n = if n == 0 then 0 else abs (x - solvedX) + abs(y - solvedY)
    where (solvedX, solvedY) = indToCoords (n-1)
          (x, y) = indToCoords i

instance Hashable Puzzle where
    hash :: Puzzle -> Int
    hash (Puzzle _ _ h _) = h

heuristic :: Puzzle -> Int
heuristic (Puzzle _ _ _ h) = h

-- generates 15 puzzle from list of tiles row by row
fromList :: [Int] -> Puzzle
fromList ns = Puzzle hole tiles (manualHash tiles) (manualHeuristic tiles)
    where tiles = listArray (0,15) ns
          hole = fromJust $ elemIndex 0 ns

solvedPuzzle :: Puzzle
solvedPuzzle = fromList ([1..15] ++ [0])

getMoves :: Puzzle -> [(Move, Int, Puzzle)] -- integer in the middle is edge length, in this case 1
getMoves puzzle = mapMaybe (getMove puzzle) [LeftM, RightM, UpM, DownM]

getMove :: Puzzle -> Move -> Maybe (Move, Int, Puzzle)
getMove (Puzzle holeInd tiles hash heur) move = if inBounds (mx, my) then Just (move, 1, newPuzzle) else Nothing
    where newPuzzle = Puzzle movedInd newTiles newHash newHeur
          newTiles = tiles // [(holeInd, movedTile), (movedInd, 0)]

          -- incremental hash and heuristic calculation, very efficient
          newHash = hash + movedTile * (16 ^ holeInd - 16 ^ movedInd)
          newHeur = heur + tileDistance holeInd movedTile - tileDistance movedInd movedTile

          movedTile = tiles ! movedInd
          movedInd = coordsToInd (mx, my)
          (mx, my) = (hx + dx, hy + dy)
          (dx, dy) = moveCoords move
          (hx, hy) = indToCoords holeInd

-- solve 15 puzzle optimally using A*
optimalSolve :: Puzzle -> Maybe (Int, String)
optimalSolve puzzle = (\(p, n, ms) -> (n, map moveChar ms)) <$> aStar puzzle (== solvedPuzzle) getMoves heuristic

-- solve 15 puzzle suboptimally by first finding position with heuristic <= 10
suboptimalSolve :: Puzzle -> Maybe (Int, String)
suboptimalSolve = solveInParts [10]

-- solve 15 puzzle in parts, by iteratively applying A* to find positions with lower and lower heuristics
solveInParts :: [Int] -> Puzzle -> Maybe (Int, String)
solveInParts steps puzzle = do
    (_, ms) <- foldl solveOnePart (Just (puzzle, [])) (steps ++ [0])
    return (length ms, map moveChar ms)

-- solveInParts, but doesn't solve until the end, returns final puzzle
partSolveInParts :: [Int] -> Puzzle -> Maybe (Puzzle, Int, String)
partSolveInParts steps puzzle = do
    (p, ms) <- foldl solveOnePart (Just (puzzle, [])) steps
    return (p, length ms, map moveChar ms)

solveOnePart :: Maybe (Puzzle, [Move]) -> Int -> Maybe (Puzzle, [Move])
solveOnePart accumMaybe goal = do
    (puzzle, ms) <- accumMaybe
    (p', _, ms') <- aStar puzzle (\p -> heuristic p <= goal) getMoves (\p -> max (heuristic p - goal) 0)
    return $ trace ("Goal achieved: " ++ show goal) (p', ms ++ ms')

-- main test function used for profiling
main :: IO ()
main = do
    print $ suboptimalSolve $ fromList [4,0,3,14,2,15,10,11,8,5,6,12,7,13,1,9]