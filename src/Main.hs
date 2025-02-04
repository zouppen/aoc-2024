{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards, TupleSections #-}
module Main where

import Control.Concurrent.STM
import Control.Monad (forM, forM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.List (intercalate)
import System.Clock
import System.Console.CmdArgs.Implicit
import System.IO (hFlush, hPutStrLn, stdout, stderr)
import Text.Printf

import Tonttu (Runner(..), Tonttu(..), niputa)
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
                     \If not set, all parts are solved. Magic value 'parser' \
                     \prints parsed puzzle input."
              , json = False &=
                help "Use JSON output instead of Haskell pretty-printing"
              }
          &= program "aoc-zouppen-2024"
          &= summary "Advent of Code solutions by Zouppen"

tontusto :: M.Map Int Tonttu
tontusto = niputa tontut

main :: IO ()
main = do
  Args{..} <- cmdArgs argsDef
  -- Check argument sanity
  case (days, input) of
    ([_], _) -> pure ()
    (_, Just _) -> fail "You must specify a single day to run when \
                        \input file is specified"
    _ -> pure ()
  let file day = case input of
        Just x -> x
        Nothing -> printf "inputs/%02d" day
      daysOrAll = case days of
        [] -> M.keys tontusto
        a  -> a
      dayList = intercalate ", " $ map show $ M.keys tontusto
      run proj = forM daysOrAll $ \day -> case M.lookup day tontusto of
        Just tonttu -> (day,) <$> (proj tonttu) (file day) part
        Nothing     -> fail $ "Not a valid day. Valid days: " <> dayList
  -- Start running
  if json
    then do allRunners <- run jsonRunner
            -- Now it's running in the background. Let's fetch results
            dayPairs <- forM allRunners $ \(day, dayRunners) -> do
              hPrintf stderr "Calculating day %d..." day
              partPairs <- forM dayRunners $ \Runner{..} -> do
                hPrintf stderr " %s" infoText
                (endTime, val) <- atomically resultAct
                pure $ let resObj = [ "result" A..= val
                                    , "time"   A..= diffSec endTime startTime
                                    ]
                       in A.fromText infoText A..= A.object resObj
              hPutStrLn stderr ""
              pure $ A.fromString (printf "%02d" day) A..= A.object partPairs
            printJSON $ A.object dayPairs
    else do allRunners <- run plainRunner
            -- Now it's running in the background. Let's fetch results
            forM_ allRunners $ \(day, dayRunners) -> do
              printf "--- Day %d ---\n" day
              forM_ dayRunners $ \Runner{..} -> do
                printf "Running %s... " infoText
                hFlush stdout
                (endTime, str) <- atomically resultAct
                printf "%-15s % 7.3f s\n" str (diffSec endTime startTime)

intervalToFrac :: TimeSpec -> Double
intervalToFrac a = 1e-9 * (fromIntegral $ toNanoSecs a)

diffSec :: TimeSpec -> TimeSpec -> Double
diffSec a b = intervalToFrac $ diffTimeSpec a b

printJSON :: A.ToJSON a => a -> IO ()
printJSON a = BL.hPut stdout $ A.encode a <> "\n"
