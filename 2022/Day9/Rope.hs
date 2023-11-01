module Rope where

import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Map qualified as M
import System.IO (readFile')

main :: IO ()
main = print . countPositions . runSim . parseInput =<< readFile' "input.txt"

countPositions :: [(a, (Int, Int))] -> Int
countPositions = length . nubOrd . map snd

parseInput :: String -> [(Char, Int)]
parseInput = map ((\[i, n] -> (head i, read n)) . words) . lines

runSim :: [(Char, Int)] -> [((Int, Int), (Int, Int))]
runSim = foldl' simulate [((0, 0), (0, 0))]

simulate :: [((Int, Int), (Int, Int))] -> (Char, Int) -> [((Int, Int), (Int, Int))]
simulate p (d, 0) = p
simulate (pos : positions) (direction, n) = simulate (pos' : pos : positions) (direction, n - 1)
  where
    dir = dirs M.! direction
    pos' = step dir pos

step :: (Int, Int) -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
step (x, y) (head@(hx, hy), tail@(tx, ty)) = (head', tail')
  where
    head'@(hx', hy') = (hx + x, hy + y)
    distance = abs (hx' - tx) > 1 || abs (hy' - ty) > 1
    tail' = if distance then head else tail

dirs :: Map Char (Int, Int)
dirs = M.fromList [('U', (0, 1)), ('D', (0, -1)), ('R', (1, 0)), ('L', (-1, 0))]
