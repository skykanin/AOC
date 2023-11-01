{-# LANGUAGE OverloadedStrings #-}
module CalorieCounting where

import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

readt :: Read a => Text -> a
readt = read . T.unpack

countCalories :: Text -> [Int]
countCalories = sortOn Down . map (sum . map readt) . map T.lines . T.splitOn "\n\n"

main :: IO ()
main = do
  file <- T.readFile "input.txt"
  let calorieCount = countCalories file
  -- Part 1
  print . head $ calorieCount
  -- Part 2
  print . sum . take 3 $ calorieCount
