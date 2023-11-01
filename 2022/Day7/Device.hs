{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoFieldSelectors #-}

module Device where

import Control.Monad.State.Strict (State, execState, get, modify', runState)
import Data.Either (rights)
import Data.Foldable (foldl', toList)
import Data.Functor (void, ($>))
import Data.Functor.Identity (Identity)
import Data.List (find, intersperse, isPrefixOf, isSuffixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Void (Void)
import GHC.Generics (Generic)
import Optics.Core hiding ((|>))
import System.IO (readFile')
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P

main :: IO ()
main = do
  input <- readFile' "input.txt"
  let files = fileSizes input

  print $ atMost 100000 files -- Part 1
  print $ dirToDelete 30000000 70000000 files -- Part 2

atMost :: Int -> Map String Int -> Int
atMost n = sum . filter (n >) . map snd . Map.toList

dirToDelete :: Int -> Int -> Map String Int -> Int
dirToDelete neededSpace diskSpace files =
  minimum $ filter (>= needed) $ map snd $ Map.toList files
  where
    used = files Map.! "/"
    needed = neededSpace - (diskSpace - used)

fileSizes :: String -> Map String Int
fileSizes input =
  runParser input ^. #files
    & merge
    & traversed %~ (sum . toListOf (folded % #size))

findKeys :: String -> [String] -> [String]
findKeys match xs = match : filter belongs xs
  where
    belongs filePath = filePath `isPrefixOf` match

insertWithIn :: Ord k => (v -> v -> v) -> [k] -> v -> Map k v -> Map k v
insertWithIn f ks v m = foldl' (\m k -> Map.insertWith f k v m) m ks

merge :: Map String [File] -> Map String [File]
merge = Map.foldlWithKey' f Map.empty
  where
    f m key el = insertWithIn (<>) (findKeys key $ Map.keys m) el m

data File = File
  { name :: String
  , size :: Int
  }
  deriving (Eq, Generic, Show)

-- The parser state for tracking the current directory and all currently explored files
data ParserState = ParserState
  { currentDirectory :: Seq String
  , files :: Map String [File]
  }
  deriving (Generic, Show)

emptyState :: ParserState
emptyState =
  ParserState
    { currentDirectory = Seq.empty
    , files = Map.empty
    }

-- Our stateful parser type
type Parser = P.ParsecT Void String (State ParserState)

runParser :: String -> ParserState
runParser = flip execState emptyState . P.runParserT parseCommands "input.txt"

cd :: Parser ()
cd = do
  P.char '$'
  P.space1
  P.string "cd"
  P.space1
  name <- P.some (P.letterChar P.<|> P.char '/' P.<|> P.char '.')
  modify' $ \ps -> case name of
    ".." ->
      ps
        & #currentDirectory %~ \case
          start :|> last -> start
          x -> x
    _ -> ps & #currentDirectory %~ (|> name)

dirName :: Parser ()
dirName = P.string "dir" *> P.space1 <* P.some P.letterChar

file :: Parser File
file = do
  size <- read <$> P.some P.digitChar
  P.space1
  name <- P.some (P.letterChar P.<|> P.char '.')
  pure $ File name size

mkDirName :: Seq String -> String
mkDirName seq =
  concat $ toList seq & _tail %~ intersperse "/"

updateFiles :: [File] -> ParserState -> ParserState
updateFiles !newFiles !state =
  state & #files %~ Map.insert (mkDirName $ view #currentDirectory state) newFiles

listItems :: Parser [Either () File]
listItems = do
  dirItem <- P.eitherP dirName file
  res <- P.optional $ P.lookAhead (P.try $ P.newline *> P.char '$')
  case res of
    Just _ -> pure [dirItem]
    Nothing -> do
      end <- P.optional $ P.lookAhead (P.try $ P.newline *> P.eof)
      case end of
        Just _ -> pure [dirItem]
        Nothing -> (dirItem :) <$> (P.newline *> listItems)

ls :: Parser ()
ls = do
  P.char '$'
  P.space1
  P.string "ls"
  P.newline
  content <- listItems
  let newFiles = rights content
  modify' $ updateFiles newFiles

parseCommands :: Parser ()
parseCommands = void $ P.sepEndBy1 (P.try cd P.<|> ls) P.newline
