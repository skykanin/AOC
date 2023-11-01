module RockPaperScissors where

import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.IO (readFile')

data Shape = Rock | Paper | Scissors
  deriving (Enum, Eq, Show)

data Result = Lose | Draw | Win
  deriving (Enum, Eq, Show)

shapeMap :: Map String Shape
shapeMap = Map.fromList
  [ ("A", Rock), ("B", Paper), ("C", Scissors)
  , ("X", Rock), ("Y", Paper), ("Z", Scissors)
  ]

resultMap :: Map String Result
resultMap = Map.fromList $ zip ["X", "Y", "Z"] [Lose .. Win]

withMap :: Map String v -> String -> (Shape, v)
withMap mapping str = fromJust $ (,) <$> Map.lookup x shapeMap <*> Map.lookup y mapping
  where
    x = take 1 str
    y = drop 2 str

shapePoints :: Shape -> Int
shapePoints shape = fromEnum shape + 1

matchPoints :: Shape -> Shape -> Int
matchPoints Rock y = case y of
  Rock -> 3
  Paper -> 6
  Scissors -> 0
matchPoints Paper y = case y of
  Rock -> 0
  Paper -> 3
  Scissors -> 6
matchPoints Scissors y = case y of
  Rock -> 6
  Paper -> 0
  Scissors -> 3

fixMatch :: Shape -> Result -> Shape
fixMatch Rock res = case res of
  Lose -> Scissors
  Draw -> Rock
  Win -> Paper
fixMatch Paper res = case res of
  Lose -> Rock
  Draw -> Paper
  Win -> Scissors
fixMatch Scissors res = case res of
  Lose -> Paper
  Draw -> Scissors
  Win -> Rock

tallyPoints :: Shape -> Shape -> Int
tallyPoints x y = shapePoints y + matchPoints x y

tallyPoints' :: Shape -> Result -> Int
tallyPoints' x y = shapePoints playerShape + matchPoints x playerShape
  where
    playerShape = fixMatch x y

main :: IO ()
main = do
  matches <- lines <$> readFile' "input.txt"
  -- Part 1
  print . sum . map (uncurry tallyPoints . withMap shapeMap) $ matches
  -- Part 2
  print . sum . map (uncurry tallyPoints' . withMap resultMap) $ matches
