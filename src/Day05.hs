{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Day05 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Aeson as A
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import GHC.Generics

import Day

task :: Day Lahjakori Int
task = Day { parser = lahjakori
           , solvers = [("part1", ShowSolver $ joululaulu True)
                       ,("part2", ShowSolver $ joululaulu False)
                       ]
           }

-- Types

data Lahjakori = Lahjakori { rules   :: Rules
                           , updates :: [Update]
                           } deriving (Show, Generic)

instance A.ToJSON Lahjakori

newtype Rules = Rules (IM.IntMap IS.IntSet) deriving (Show, Generic)

instance A.ToJSON Rules

newtype Update = Update [Int] deriving (Show, Generic)

instance A.ToJSON Update

-- Parsing

rule :: Parser (Int, IS.IntSet)
rule = do
  a <- decimal
  _ <- char '|'
  b <- decimal
  _ <- endOfLine
  pure (a, IS.singleton b)

update :: Parser Update
update = do
  a <- decimal `sepBy` char ','
  _ <- endOfLine
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
comparator rules a b = case (aFirst, bFirst) of
  (True, False)  -> LT
  (False, True)  -> GT
  (False, False) -> EQ
  _ -> error "Ei ole kilttiä tämä ollenkaan"
  where thisRule = IM.lookup b rules
        bFirst = case thisRule of
          Nothing -> False
          Just s -> IS.member a s
        aFirst = case thisRule of
          Nothing -> False
          Just s -> IS.member b s

-- |Get a middle value of a list or error if the list is odd-length.
middle :: [a] -> a
middle xs = if oddity == 0
            then error "Not odd number"
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
