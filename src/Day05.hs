{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Day05 where

import Control.Applicative
import Data.Aeson (ToJSON)
import Data.Attoparsec.ByteString.Char8
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import GHC.Generics

import Day

task :: Day Lahjakori Int
task = Day { parser  = lahjakori
           , solvers = [ part1 $ joululaulu True
                       , part2 $ joululaulu False
                       ]
           }

-- Types

data Lahjakori = Lahjakori { rules   :: Rules
                           , updates :: [Update]
                           } deriving (Show, Generic)

instance ToJSON Lahjakori

newtype Rules = Rules (IM.IntMap IS.IntSet) deriving (Show, Generic)

instance ToJSON Rules

newtype Update = Update [Int] deriving (Show, Generic)

instance ToJSON Update

-- Parsing

rule :: Parser (Int, IS.IntSet)
rule = do
  a <- decimal
  _ <- char '|'
  b <- decimal
  endOfLine
  pure (a, IS.singleton b)

update :: Parser Update
update = do
  a <- decimal `sepBy` char ','
  endOfLine
  pure $ Update a

lahjakori :: Parser Lahjakori
lahjakori = do
  rules <- Rules . IM.fromListWith (<>) <$> many rule
  endOfLine
  updates <- many update
  endOfInput
  pure $ Lahjakori{..}

-- Business logic

comparator :: IM.IntMap IS.IntSet -> Int -> Int -> Ordering
comparator rules a b = case (isFirst a b, isFirst b a) of
  (True, False)  -> LT
  (False, True)  -> GT
  (False, False) -> EQ
  _ -> error "Ei ole kilttiä tämä ollenkaan"
  where isFirst n1 n2 = case IM.lookup n1 rules of
          Nothing -> False
          Just s -> IS.member n2 s

-- |Get a middle value of a list or an error if the list is odd-length.
middle :: [a] -> a
middle xs = if oddity == 0
            then error "List length is even"
            else xs !! mid
  where (mid, oddity) = quotRem (length xs) 2

-- |Uses our comparator to sort the list based on the elf rules. If
-- which is True, then only lists which stay equal are counted, on
-- False (Part 2) the lists which differ after sorting are counted.
validMid :: Bool -> Rules -> Update -> Int
validMid which (Rules rules) (Update xs) =
  let fixed = sortBy (comparator rules) xs
  in if (xs == fixed) == which
     then middle fixed
     else 0

joululaulu :: Bool -> Lahjakori -> Int
joululaulu which Lahjakori{..} = sum $ map (validMid which rules) updates
