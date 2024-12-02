{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import System.Console.CmdArgs.Implicit
import qualified Data.Map.Strict as M
import Data.List (intercalate)

import Tontut

data Args = Args { day    :: Int
                 , input  :: FilePath
                 , part   :: [String]
                 , json   :: Bool
                 } deriving (Show, Data, Typeable)

argsDef :: Args
argsDef = Args{ day = def &= typ "DAY" &= argPos 0
              , input = def &= typFile &= argPos 1
              , part = def &=
                help "Solve specific part. Can be applied multiple times. \
                     \If none, all parts are solved."
              , json = False &=
                help "Use JSON output instead of Haskell pretty-printing"
              }
          &= summary "Advent of Code solutions by Zouppen"

main :: IO ()
main = do
  Args{..} <- cmdArgs argsDef
  case M.lookup day tontut of
    Just (Tonttu t) -> t json input part
    Nothing         -> fail $ "Not a valid day. Valid days: " <> dayList
      where dayList = intercalate ", " $ map show $ M.keys tontut
