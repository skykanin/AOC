module Tree where

import Data.List (findIndex, maximumBy, transpose)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import System.IO (readFile')
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  rows <- map (map (read . pure)) . lines <$> readFile' "input.txt"
  print . countVisible . applyToMatrix visible $ initPaint rows -- Part 1
  print . bestView . applyToMatrix viewing $ initView rows -- Part 2

countVisible :: [[(Int, Bool)]] -> Int
countVisible = length . filter snd . concat

bestView :: [[(Int, Int)]] -> Int
bestView = snd . maximumBy (comparing snd) . concat

applyToMatrix :: ([a] -> [a]) -> [[a]] -> [[a]]
applyToMatrix f = map row . transpose . map row
  where
    row = f . reverse . f

initPaint :: [[Int]] -> [[(Int, Bool)]]
initPaint = map (map (,False))

initView :: [[Int]] -> [[(Int, Int)]]
initView = map (map (,1))

visible :: [(Int, Bool)] -> [(Int, Bool)]
visible = snd . foldr f (Nothing, [])
  where
    f (i, seen) (tallest, l) = case tallest of
      Nothing -> (Just i, (i, True) : l)
      Just curMax
        | i > curMax -> (Just i, (i, True) : l)
        | otherwise -> (Just curMax, (i, seen) : l)

viewing :: [(Int, Int)] -> [(Int, Int)]
viewing = foldr f []
  where
    f (i, viewScore) acc = (i, seenShorter * viewScore) : acc
      where
        seen = length acc
        seenShorter = maybe seen (1 +) (findIndex ((>= i) . fst) acc)
