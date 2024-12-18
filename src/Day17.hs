{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings #-}
module Day17 where

import Control.Monad (guard)
import Data.Attoparsec.ByteString.Char8 hiding (takeWhile)
import qualified Data.Array as A
import Data.Bits
import Data.List (intercalate, tails)

import Day

task :: Day CPU Int
task = Day { parser  = cpuParse
           , solvers = [ ("part1", StringSolver $ intercalate "," . map show . runAndOutput)
                       , part 2 $ findQuine
                       ]
           }

data CPU = CPU { regA    :: Int
               , regB    :: Int
               , regC    :: Int
               , ip      :: Int
               , progmem :: Progmem
               , output  :: [Int]
               } deriving (Show, Generic)

newtype Progmem = Progmem (A.Array Int Int) deriving (Show)

instance ToJSON CPU

instance ToJSON Progmem where
  toJSON (Progmem a) = toJSON $ A.elems a

-- Parser

cpuParse :: Parser CPU
cpuParse = do
  _ <- string "Register A: "
  regA <- decimal
  _ <- string "\nRegister B: "
  regB <- decimal
  _ <- string "\nRegister C: "
  regC <- decimal
  _ <- string "\n\nProgram: "
  prog <- decimal `sepBy` char ','
  _ <- string "\n"
  endOfInput
  pure CPU{ progmem = Progmem $ A.listArray (0, length prog-1) prog
          , ip = 0, output = mempty, ..
          }

-- |Run one instruction on the Elf CPU and return the new CPU state.
step :: CPU -> CPU
step cur@CPU{..} = case prog A.! ip of
  0 -> n{ regA = regA .>>. combo }
  1 -> n{ regB = regB `xor` lit }
  2 -> n{ regB = combo .&. 0x7 }
  3 -> if regA == 0
       then n -- Let IP increase
       else n{ ip = lit }
  4 -> n{ regB = regB `xor` regC }
  5 -> n{ output = (combo .&. 0x7) : output }
  6 -> n{ regB = regA .>>. combo }
  7 -> n{ regC = regA .>>. combo }
  _ -> error "Illegal instruction"
  where n = cur{ ip = ip+2 }
        Progmem prog = progmem
        lit = prog A.! (ip+1)
        combo = case prog A.! (ip+1) of
          4 -> regA
          5 -> regB
          6 -> regC
          7 -> error "illegal combo operand"
          a -> a

runToHalt :: CPU -> [CPU]
runToHalt = takeWhile isRunning . iterate step
  where isRunning m = let Progmem prog = progmem m
                          (_, end) = A.bounds prog
                      in ip m < end

runAndOutput :: CPU -> [Int]
runAndOutput = reverse . output . last . runToHalt

-- Part 2

-- |Tries to reconstruct valid inputs from given outputs, assuming
-- there is a specific bit shift of three.
back :: Eq a => (Int -> a) -> [Int] -> a -> [Int]
back test cs expect = do
  c <- cs
  x <- [0..(bit bitWidth)-1]
  let a = c .<<. bitWidth .|. x
  guard $ test a == expect
  pure a
  where bitWidth = 3

findQuine :: CPU -> Int
findQuine cpu = minimum $ foldl (back tester) [0] backList
  where tester x = runAndOutput $ cpu{regA = x}
        backList = tail $ reverse $ tails $ A.elems prog
        Progmem prog = progmem cpu
