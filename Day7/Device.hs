{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Device where

import Control.Monad.State.Strict (State, execState, get, modify', runState)
import Control.Monad.Trans (lift)
import Data.Either (rights)
import Data.Foldable (toList)
import Data.Functor (void, ($>))
import Data.Functor.Identity (Identity)
import Data.List (intersperse)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace (traceId, traceShow, traceShowId)
import GHC.Generics (Generic)
import Optics.Core (over, view, (%~), (&), (^.), _tail)
import System.IO (readFile')
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

main :: IO ()
main = do
  input <- readFile' "test.txt"
  print input
  print . runCmdParser input $ emptyState

data File = File
  { name :: String
  , size :: Int
  }
  deriving (Eq, Generic, Show)

-- The parser state for tracking the current directory
-- and all the explored files
data ParserState = ParserState
  { currentDirectory :: Seq String
  , files :: Map String [File]
  }
  deriving (Generic, Show)

-- Our stateful parser type
type Parser = ParsecT Void String (State ParserState)

-- runCmdParser :: String -> ParserState -> ParserState
runCmdParser i s = runState s <$> runParserT' parseCommands (pure i)

cd :: Parser ()
cd = do
  char '$'
  space1
  string "cd"
  space1
  name <- some (letterChar <|> char '/')
  traceId "CD PARSER" `seq`
    modify' $ \ps -> case name of
      ".." ->
        ps
          & #currentDirectory %~ \case
            start :|> last -> start
            x -> x
      _ -> ps & #currentDirectory %~ (|> name)

dirName :: Parser ()
dirName = string "dir" *> space1 <* some letterChar

file :: Parser File
file = do
  size <- read <$> some digitChar
  space1
  name <- some (letterChar <|> char '.')
  pure $ File name size

emptyState =
  ParserState
    { currentDirectory = Seq.empty
    , files = Map.empty
    }

mkDirName :: Seq String -> String
mkDirName seq =
  concat $ toList seq & _tail %~ intersperse ","

updateFiles :: [File] -> ParserState -> ParserState
updateFiles !newFiles !state =
  traceShowId $
    state & #files %~ Map.insert (mkDirName $ view #currentDirectory state) newFiles

ls :: Parser ()
ls = do
  char '$'
  space1
  string "ls"
  newline
  content <- sepEndBy1 (Left <$> dirName <|> Right <$> file) newline
  let newFiles = rights content
  traceId "LS PARSER" `seq` (modify' $ updateFiles newFiles)

parseCommands :: Parser ()
parseCommands = void $ sepEndBy1 (try cd <|> ls) newline
