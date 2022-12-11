{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module CPU where

import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Void (Void)
import GHC.Generics (Generic)
import Optics.Core ((^.))
import System.IO (readFile')
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as PL
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  i <- readFile' "input.txt"
  pPrint . calcSignal . simulate . fromJust . P.parseMaybe cmds $ i -- Part 1

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
