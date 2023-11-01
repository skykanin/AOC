module CampCleanup where

import Control.Applicative (some)
import Data.Foldable (traverse_)
import System.IO (readFile')
import Text.ParserCombinators.ReadP

digit :: ReadP Char
digit = satisfy (`elem` "0123456789")

num :: ReadP Int
num = read <$> some digit

range :: ReadP (Int, Int)
range = (,) <$> num <* char '-' <*> num

rangePair :: ReadP ((Int, Int), (Int, Int))
rangePair = (,) <$> range <* char ',' <*> range

ranges :: ReadP [((Int, Int), (Int, Int))]
ranges = sepBy1 rangePair (char '\n')

runParser :: ReadP a -> String -> a
runParser parser = fst . last . readP_to_S parser

contains :: (Int, Int) -> (Int, Int) -> Bool
contains (a, b) (c, d) = c >= a && b >= d || a >= c && d >= b

overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps (a, b) (c, d) = a <= d && b >= c

countSections :: ((Int, Int) -> (Int, Int) -> Bool) -> String -> Int
countSections pred = length . filter (uncurry pred) . runParser ranges

main :: IO ()
main = do
  file <- readFile' "input.txt"
  traverse_ (print . ($ file)) [countSections contains, countSections overlaps]
