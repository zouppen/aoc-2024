{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import Control.Concurrent.STM
import Control.Monad (forM, forM_, zipWithM, zipWithM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.List (intercalate)
import System.IO (hFlush, hPutStrLn, stdout, stderr)
import System.Console.CmdArgs.Implicit
import Text.Printf

import Tonttu (Runner(..))
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
  let file day = case input of
        Just x -> x
        Nothing -> printf "inputs/%02d" day
      daysOrAll = case days of
        [] -> M.keys tontut
        a  -> a
      dayList = intercalate ", " $ map show $ M.keys tontut
      dayFail = fail $ "Not a valid day. Valid days: " <> dayList
  -- Start running
  if json
    then do allRunners <- forM daysOrAll $ \day -> case M.lookup day tontut of
              Just Tonttu{..} -> jsonRunner (file day) part
              Nothing         -> dayFail
            o <- let act day dayRunners = do
                       hPrintf stderr "Calculating day %d..." day
                       pairs <- forM dayRunners $ \Runner{..} -> do
                         hPrintf stderr " %s" infoText
                         val <- atomically result
                         pure $ A.fromText infoText A..= val
                       hPutStrLn stderr ""
                       pure $ A.fromString (show day) A..= A.object pairs
                 in zipWithM act daysOrAll allRunners
            printJSON $ A.object o
    else do allRunners <- forM daysOrAll $ \day -> case M.lookup day tontut of
              Just Tonttu{..} -> plainRunner (file day) part
              Nothing         -> dayFail
            let act day dayRunners = do
                  printf "--- Day %d ---\n" day
                  forM_ dayRunners $ \Runner{..} -> do
                    T.hPutStr stdout infoText
                    hFlush stdout
                    str <- atomically result
                    putStrLn str
                  pure ()
              in zipWithM_ act daysOrAll allRunners

printJSON :: A.ToJSON a => a -> IO ()
printJSON a = BL.hPut stdout $ A.encode a <> "\n"
