{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import Control.Monad (forM, forM_)
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.List (intercalate)
import System.IO (stdout)
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
    then do values <- forM daysOrAll $ \day -> case M.lookup day tontut of
              Just Tonttu{..} -> jsonRunner (file day) part
              Nothing         -> dayFail
            printJSON $ M.fromList $ zip daysOrAll values
    else do forM_ daysOrAll $ \day -> do
              printf "--- Day %d ---\n" day
              case M.lookup day tontut of
                Just Tonttu{..} -> plainRunner (file day) part
                Nothing         -> dayFail

printJSON :: A.ToJSON a => a -> IO ()
printJSON a = BL.hPut stdout $ A.encode a <> "\n"
