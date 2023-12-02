{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Control.Monad.Combinators qualified as C
import Data.Tuple
import Control.Applicative
import Data.Char
import Data.Functor
import Data.Foldable
import Data.Traversable
import Data.Map qualified as M
import Data.Map.Merge.Strict qualified as M
import Data.Void (Void)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

type Parser = P.Parsec Void String
data Color = Red | Green | Blue deriving (Eq, Ord, Show)

data Game = MkGame
  { index :: Int
  , red :: Int
  , green :: Int
  , blue :: Int
  } deriving Show

integer :: Parser Int
integer = read <$> P.takeWhile1P (Just "integer") isDigit

games :: Parser [Game]
games = C.sepEndBy1 game P.newline

game :: Parser Game
game = toGame <$> liftA2 (,) index (foldl' merge M.empty <$> C.sepBy1 roll (P.char ';'))
  where
    index = P.string "Game" *> P.space *> integer <* P.char ':'
    merge = M.merge M.preserveMissing' M.preserveMissing' (M.zipWithMatched (const max))
    toGame (i, dict) = MkGame i (dict M.! Red) (dict M.! Green) (dict M.! Blue)

cube :: Parser (Color, Int)
cube = liftA2 (swap .: (,)) integer $ P.space *> (red P.<|> green P.<|> blue)
  where
    (.:) = (.) . (.)
    red = P.string "red" $> Red
    green = P.string "green" $> Green
    blue = P.string "blue" $> Blue

roll :: Parser (M.Map Color Int)
roll = M.fromList <$> (P.space *> C.sepBy1 cube (P.string ", "))

part1 :: [Game] -> Int
part1 = sum . map (.index) . filter (\g -> all ($ g) [(12 >=) . (.red), (13 >=) . (.green), (14 >=) . (.blue)])

part2 :: [Game] -> Int
part2 = sum . map (\g -> product [g.red, g.green, g.blue])

main :: IO ()
main = do
  fileContent <- readFile file
  case P.parse games file fileContent of
    Left _ -> putStrLn "Couldn't parse input"
    Right gs -> do
      print $ part1 gs
      print $ part2 gs
  where
    file = "./input.txt"
