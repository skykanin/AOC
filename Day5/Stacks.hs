module Stacks where

import Data.Foldable (foldl')
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List (transpose)
import Data.Maybe (catMaybes, fromJust)
import Data.Void (Void)
import System.IO (readFile')
import Text.Megaparsec
  ( Parsec
  , anySingle
  , manyTill
  , parse
  , parseMaybe
  , satisfy
  , sepBy1
  , skipCount
  , some
  , (<|>)
  )
import Text.Megaparsec.Char (char, letterChar, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

main :: IO ()
main = do
  file <- readFile' "input.txt"
  let (stacks, moves) = runParser file
      run crane = getTop $ simulate crane moves stacks

  putStrLn $ run CrateMover9000 -- Part 1
  putStrLn $ run CrateMover9001 -- Part 2

-- Simulating

data Crane = CrateMover9000 | CrateMover9001

getTop :: IntMap [Char] -> [Char]
getTop = map snd . IM.toAscList . IM.map head

simulate :: Crane -> [(Int, Int, Int)] -> IntMap [Char] -> IntMap [Char]
simulate crane cmds crates = foldl' (flip $ uncurry3 (move crane)) crates cmds
  where
    uncurry3 f (a, b, c) = f a b c

move :: Crane -> Int -> Int -> Int -> IntMap [Char] -> IntMap [Char]
move crane amount from to m =
  IM.update (Just . mover crane) to $ IM.insert from newVal m
  where
    (toTake, newVal) = splitAt amount $ m IM.! from
    mover CrateMover9000 = (reverse toTake <>)
    mover CrateMover9001 = (toTake <>)

-- Parsing

type Parser = Parsec Void String

crate :: Parser Char
crate = char '[' *> letterChar <* char ']'

crateSlot :: Parser (Maybe Char)
crateSlot = (Just <$> crate) <|> (Nothing <$ string "   ")

crateRow :: Parser [Maybe Char]
crateRow = (crateSlot `sepBy1` char ' ') <* newline

crateStacks :: Parser (IntMap [Char])
crateStacks = do
  rows <- some crateRow
  let columns = transpose rows
      stacks = catMaybes <$> columns
  pure . IM.fromAscList $ zip [1 ..] stacks

moveCmd :: Parser (Int, Int, Int)
moveCmd = do
  amount <- string "move " *> decimal
  from <- string " from " *> decimal
  to <- string " to " *> decimal
  pure (amount, from, to)

stacksAndMoves :: Parser (IntMap [Char], [(Int, Int, Int)])
stacksAndMoves = do
  stacks <- crateStacks
  skipCount 2 $ manyTill anySingle newline
  moves <- some $ moveCmd <* space
  pure (stacks, moves)

runParser :: String -> (IntMap [Char], [(Int, Int, Int)])
runParser = either (error . show) id . parse stacksAndMoves "puzzle"
