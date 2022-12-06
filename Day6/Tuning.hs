module Tuning where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set qualified as Set
import System.IO (readFile')

main :: IO ()
main = do
  input <- readFile' "input.txt"
  let (packetSize, messageSize) = (4, 14)
      solve size = findUnique size . window size . zip [1 ..]
  print $ solve packetSize input -- Part 1
  print $ solve messageSize input -- Part 2

findUnique :: Int -> [[(Int, Char)]] -> Maybe Int
findUnique size = listToMaybe . mapMaybe unique
  where
    unique xs
      | Set.size (Set.fromList $ map snd xs) == size = Just . maximum $ map fst xs
      | otherwise = Nothing

-- Paramorphism fold
para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para _ z [] = z
para f z (x : xs) = f x xs (para f z xs)

-- List of sliding windows of size `n`
window :: Int -> [a] -> [[a]]
window n = para f []
  where
    f x xs z = take n (x : xs) : z
