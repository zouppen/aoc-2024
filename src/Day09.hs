{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Day09 where

import Control.Applicative
import Data.Aeson (ToJSON)
import Data.Attoparsec.ByteString.Char8
import Data.List (sortOn)
import qualified Data.IntMap as IM
import GHC.Generics
import Text.Printf

import Day

task :: Day [File] Int
task = Day { parser  = parseAll
           , solvers = [ part1 $ joulutorttu . defrag 0 . toDisk
                       , part2 $ joulutorttu . toDisk . pickAndPlaces
                       ]
           }

-- |Curry ei todellakaan kuulu joulutorttuun!
joulutorttu :: IM.IntMap IM.Key -> IM.Key
joulutorttu = sum . map (uncurry (*)) . IM.toList

-- Types

data ParsePos = ParsePos { parsePos :: Int
                         , parseId  :: Int
                         } deriving (Show)

data File = File { fileId   :: Int
                 , filePos  :: Int
                 , fileSize :: Int
                 } deriving (Show, Eq, Generic)

instance ToJSON File

--Parsers

digitVal :: Parser Int
digitVal = do
  c <- digit
  pure $ fromEnum c - fromEnum '0'

parseAll :: Parser [File]
parseAll = do
  start <- parseFile ParsePos{parsePos = 0, parseId = 0}
  xmas <- carryMany parsePair [start]
  endOfLine
  endOfInput
  pure xmas

carryMany :: (Monad m, Alternative m) => (a -> m a) -> [a] -> m [a]
carryMany act (a:acc) = do
  may <- fmap Just (act a) <|> pure Nothing
  case may of
    Nothing -> pure $ a:acc
    Just b  -> carryMany act (b:a:acc)
carryMany _ _ = error "carryMany must be called with a seed value"

parsePair :: File -> Parser File
parsePair File{..} = do
  free <- digitVal
  parseFile ParsePos{ parsePos = filePos + fileSize + free
                    , parseId  = fileId + 1
                    }

parseFile :: ParsePos -> Parser File
parseFile ParsePos{..} = do
  fileSize <- digitVal
  pure File { fileId  = parseId
            , filePos = parsePos
            , ..
            }

-- BEGIN DEFRAG.EXE

fileToDisk :: File -> IM.IntMap Int
fileToDisk File{..} = IM.fromList [ (filePos + i, fileId)
                                  | i <- [0..fileSize-1]
                                  ]

-- |Converts file list to disk layout, with position as a key, and the
-- file ID as a value.
toDisk :: [File] -> IM.IntMap Int
toDisk = mconcat . map fileToDisk

defrag :: IM.Key -> IM.IntMap Int -> IM.IntMap Int
defrag i m = case IM.maxViewWithKey m of
  Just ((lastI, lastVal), newMap) ->
    if i > lastI
    then m
    else if IM.member i m
         then defrag (i+1) m
         else defrag (i+1) $ IM.insert i lastVal newMap -- Swap
  Nothing -> error "Defrag failed. Run chkdsk first!"

pickAndPlaces :: [File] -> [File]
pickAndPlaces fs = loop (peekHigh fs) fs
  where loop (-1) = id
        loop i = loop (i-1) . pickAndPlace i

pickAndPlace :: Int -> [File] -> [File]
pickAndPlace i xs = tail $ place stopAt file (pole:remList)
  where
    pole = File (-1) 0 0 -- Makes start condition to work, removed with `tail`.
    (file, remList) = case idAndPositionSort i xs of
      Just a -> a
      Nothing -> error $ printf "Given id %d not found" i
    -- In the puzzle there's no in-place move. If you want to enable
    -- it, set stopAt = filePos file
    stopAt = filePos file - fileSize file

place :: Int -> File -> [File] -> [File]
place stopAt x (a:rest@(b:_)) =
  -- Placing in a gap, if any
  let aEnd = filePos a + fileSize a
  in if stopAt < aEnd
     then a : x : rest                      -- Drop it wherever
     else if aEnd + fileSize x <= filePos b
          then a : x{filePos = aEnd} : rest -- Drop it after previous
          else a : place stopAt x rest      -- Continue
place stopAt x [a] =
  let aEnd = filePos a + fileSize a
      newX = if stopAt < aEnd
             then x                         -- Drop it wherever
             else x{filePos = aEnd}         -- Drop it after previous
  in [a, newX]
place _ _ [] = error "Undefined corner case"

-- |Sorting so that we get the specific file id and list sorted by
-- file position on single run.
idAndPositionSort :: Int -> [File] -> Maybe (File, [File])
idAndPositionSort i xs =
  let notFileId File{..} = fileId /= i
      projPair f g x = (f x, g x)
  in case sortOn (projPair notFileId filePos) xs of
       (a:b:bs) -> if fileId a == i && fileId b /= i
                   then Just (a, b:bs)
                   else Nothing
       [a]      -> if fileId a == i
                   then Just (a, [])
                   else Nothing
       []       -> Nothing

peekHigh :: [File] -> Int
peekHigh (File{..}:_) = fileId
peekHigh _ = error "Empty file list"
