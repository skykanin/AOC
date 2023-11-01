{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module CPU where

import Data.Foldable (Foldable (toList), foldl', traverse_)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Void (Void)
import GHC.Generics (Generic)
import Optics.Core (view, (^.))
import System.IO (readFile')
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as PL
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  i <- readFile' "input.txt"
  let simRes = simulate . fromMaybe (error "Invalid input") . P.parseMaybe cmds $ i

  pPrint . calcSignal $ simRes -- Part 1
  traverse_ print . drawPicture . view #registers $ simRes -- Part 2

data Cmd
  = NoOp
  | Add Char Int
  deriving (Show)

data SimState = SimState
  { register :: Int
  , registers :: Seq Int
  }
  deriving (Show, Generic)

type Parser = P.Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = PL.lexeme P.space

cmd :: Parser Cmd
cmd = add 'x' P.<|> noop
  where
    noop = NoOp <$ P.string "noop"
    add c = do
      P.string "add"
      char <- lexeme $ P.char c
      num <- PL.signed (pure ()) (read <$> P.some P.numberChar)
      pure $ Add char num

calcSignal :: SimState -> Int
calcSignal s = sum $ zipWith (*) idx (map ($ r) idxQ)
  where
    r = s ^. #registers
    idxQ = map (flip Seq.index . subtract 1) idx
    idx = [20, 60, 100, 140, 180, 220]

cmds :: Parser [Cmd]
cmds = P.sepEndBy1 cmd P.newline

simulate :: [Cmd] -> SimState
simulate = foldl' tick SimState {register = 1, registers = Seq.singleton 1}
  where
    tick SimState {..} cmd = SimState {register = register', registers = registers'}
      where
        registers' = case cmd of
          NoOp -> registers Seq.|> register
          Add _ i -> registers Seq.|> register Seq.|> (register + i)
        register' = case registers' of (_ Seq.:|> l) -> l

chunksOf :: Int -> Seq a -> Seq [a]
chunksOf _ Seq.Empty = Seq.Empty
chunksOf n xs = toList (Seq.take n xs) Seq.<| chunksOf n (Seq.drop n xs)

drawPicture :: Seq Int -> Seq String
drawPicture = chunksOf 40 . Seq.mapWithIndex draw
  where
    draw idx x
      | (idx `mod` 40) `elem` [x - 1, x, x + 1] = '#'
      | otherwise = '.'
