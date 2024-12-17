{-# LANGUAGE DeriveGeneric, RecordWildCards, TupleSections #-}
module Day16 where

import Control.Applicative
import Control.Monad (guard)
import Data.Attoparsec.ByteString.Char8 hiding (takeWhile)
import qualified Data.Array as A
import Data.Bits
import Data.List (intercalate)

import Day
import Grid

-- pt 1 ans
-- intercalate "," $ map show $ reverse $ output $ last $ run startState

startState :: CPU
startState = CPU 64751475 0 0 0 (A.listArray (0, 15) [2,4,1,2,7,5,4,5,1,3,5,5,0,3,3,0]) []

exampleState = CPU 729 0 0 0 (A.listArray (0,5) [0,1,5,4,3,0]) []

data CPU = CPU { regA    :: Int
               , regB    :: Int
               , regC    :: Int
               , ip      :: Int
               , program :: A.Array Int Int
               , output  :: [Int]
               } deriving (Show)

step :: CPU -> CPU
step m = case program m A.! ip m of
  0 -> n{ regA = regA m `div` (2^(combo m)) }
  1 -> n{ regB = regB m `xor` lit m }
  2 -> n{ regB = combo m .&. 0x7 }
  3 -> if regA m == 0
       then n -- Let IP increase
       else m{ ip = lit m }
  4 -> n{ regB = regB m `xor` regC m }
  5 -> n{ output = (combo m .&. 0x7) : output m }
  6 -> n{ regB = regA m `div` (2^(combo m)) }
  7 -> n{ regC = regA m `div` (2^(combo m)) }
  _ -> error "slnt"
  where n = m{ ip = ip m + 2}

isRunning :: CPU -> Bool
isRunning m = let (_, end) = A.bounds $ program m
              in ip m < end

run :: CPU -> [CPU]
run = takeWhile isRunning . (iterate step)

runAndOutput = reverse . output . last . run

lit :: CPU -> Int
lit m = program m A.! (ip m+1)

combo :: CPU -> Int
combo m = case program m A.! (ip m+1) of
  4 -> regA m
  5 -> regB m
  6 -> regC m
  7 -> error "illegel combo operand"
  n -> n
