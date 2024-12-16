{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Day09 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.List (sortOn)
import qualified Data.IntMap as IM

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

-- |Runs the Part 1 algorithm.
defrag :: IM.Key -> IM.IntMap Int -> IM.IntMap Int
defrag i m = case IM.maxViewWithKey m of
  Just ((lastI, lastVal), newMap) ->
    if i > lastI
    then m
    else if IM.member i m
         then defrag (i+1) m
         else defrag (i+1) $ IM.insert i lastVal newMap -- Swap
  Nothing -> error "Defrag failed. Run chkdsk first!"

-- |Runs the Part 2 algorithm. Assumption: highest fileId is first!
pickAndPlaces :: [File] -> [File]
pickAndPlaces fs = loop (peekHigh fs) fs
  where loop (-1) = id
        loop i = loop (i-1) . pickAndPlace i
        peekHigh (File{..}:_) = fileId
        peekHigh _ = error "Empty file list"

pickAndPlace :: Int -> [File] -> [File]
pickAndPlace i xs = place stopAt file 0 remList
  where
    (file, remList) = case idAndPositionSort i xs of
      Just a -> a
      Nothing -> error $ "id not found: " <> show i
    -- In the puzzle there's no in-place move. If you want to enable
    -- it, set stopAt = filePos file
    stopAt = filePos file - fileSize file

place :: Int -> File -> Int -> [File] -> [File]
place stopAt x leftBound rest =
  let dropXLeft = x{filePos = leftBound} : rest
      rightSide File{..} = filePos + fileSize
  in if stopAt < leftBound
     then x : rest  -- We're too right
     else case rest of
            (b:bs) -> if leftBound + fileSize x <= filePos b
                      then dropXLeft
                      else b : place stopAt x (rightSide b) bs -- Keep going
            [] -> dropXLeft

-- |Sorting so that we get the specific file id and list sorted by
-- file position in a single run.
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
