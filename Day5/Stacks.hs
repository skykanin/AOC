module Stacks where

import Control.Applicative (liftA2, some)
import Data.Bifunctor (bimap)
import Data.Char (isAlpha)
import Data.Foldable (foldl')
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import System.IO (readFile')
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  file <- readFile' "input.txt"
  let (x, y) = bimap init tail . break null . lines $ file
      stacks = mkStacks x
      cmds = map (parseCmd . filter (/= ' ')) y
      run crane = getTop $ simulate crane cmds stacks

  putStrLn $ run CrateMover9000 -- Part 1
  putStrLn $ run CrateMover9001 -- Part 2

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

parseCmd :: String -> (Int, Int, Int)
parseCmd = fst . head . readP_to_S parseMoveCmd

parseMoveCmd :: ReadP (Int, Int, Int)
parseMoveCmd = do
  string "move"
  amount <- int
  string "from"
  from <- int
  string "to"
  to <- int
  eof
  pure (amount, from, to)

int :: ReadP Int
int = read <$> some digit
  where
    digit = satisfy (`elem` "0123456789")

mkStacks :: [String] -> IntMap [Char]
mkStacks = foldr addLine IM.empty

addLine :: String -> IntMap [Char] -> IntMap [Char]
addLine s m = foldl' f m $ zip [0 ..] s
  where
    f m (i, c)
      | isAlpha c = IM.alter insertOrCreate (1 + (i - 1) `div` 4) m
      | otherwise = m
      where
        insertOrCreate Nothing = Just [c]
        insertOrCreate (Just oldVal) = Just $ c : oldVal
