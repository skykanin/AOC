module Rucksack where

import Data.List (foldl1')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import System.IO (readFile')

priorities :: Map Char Int
priorities = Map.fromList $ zip ['a' .. 'z'] [1 ..] <> zip ['A' .. 'Z'] [27 ..]

duplicates :: [(String, String)] -> [Char]
duplicates =
  map
    ( head . Set.toList
        . (uncurry Set.intersection . both Set.fromList)
    )
 where
  both f (x, y) = (f x, f y)

calcPriority :: [Char] -> [Int]
calcPriority = mapMaybe (`Map.lookup` priorities)

groups :: [String] -> [(String, String)]
groups = map (\x -> splitAt (length x `div` 2) x)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

findBadges :: [[String]] -> [Char]
findBadges = map (head . Set.toList . foldl1' Set.intersection . map Set.fromList)

main :: IO ()
main = do
  file <- readFile' "input.txt"
  let rucksacks = lines file
  -- Part 1
  print . sum . calcPriority . duplicates . groups $ rucksacks
  -- Part 2
  print . sum . calcPriority . findBadges . chunksOf 3 $ rucksacks
