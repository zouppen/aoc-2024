{-# LANGUAGE DeriveDataTypeable, RecordWildCards, BangPatterns #-}
module Main where

import Control.Monad (forM_, unless)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.List (intercalate)
import System.Console.CmdArgs.Implicit
import Text.Printf

import Tontut

data Args = Args { days    :: [Int]
                 , input  :: Maybe FilePath
                 , part   :: [Text]
                 , json   :: Bool
                 } deriving (Show, Data, Typeable)

argsDef :: Args
argsDef = Args{ days = def &= typ "DAY" &= args
              , input = def &= typFile &=
                help "File name to read for puzzle input, default is zero-padded \
                     \day under inputs/ subdirectory, e.g. inputs/02"
              , part = def &=
                help "Solve specific part. Can be applied multiple times. \
                     \If not set, all parts are solved. Magic value 'input' \
                     \prints puzzle input after parsing."
              , json = False &=
                help "Use JSON output instead of Haskell pretty-printing"
              }
          &= program "aoc-zouppen-2024"
          &= summary "Advent of Code solutions by Zouppen"

main :: IO ()
main = do
  Args{..} <- cmdArgs argsDef
  -- Check argument sanity
  case (days, input) of
    ([_], _) -> pure ()
    (_, Just _) -> fail "You must specify a single day to run when \
                        \input file is specified"
    _ -> pure ()
  -- Start running
  forM_ days $ \day -> do
    unless json $ printf "-- Day %d ---\n" day
    let !file = case input of
          Just x -> x
          Nothing -> printf "inputs/%02d" day
    case M.lookup day tontut of
      Just (Tonttu t) -> t json file part
      Nothing         -> fail $ "Not a valid day. Valid days: " <> dayList
        where dayList = intercalate ", " $ map show $ M.keys tontut
