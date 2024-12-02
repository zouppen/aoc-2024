{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import System.Console.CmdArgs.Implicit
import qualified Data.Map.Strict as M
import Data.List (intercalate)

import Tontut

data Dump = NoDump
          | HaskellDump
          | JsonDump deriving (Show, Data)

data Args = Args { day   :: Int
                 , input :: FilePath
                 , parts :: [String]
                 , dump  :: Dump
                 } deriving (Show, Data, Typeable)

argsDef :: Args
argsDef = Args{ day = def &= typ "DAY" &= argPos 0
              , input = def &= typFile &= argPos 1
              , parts = [] &= name "part" &=
                help "Solve specific part. Can be applied multiple times. \
                     \If none, all parts are solved."
              , dump = NoDump &= name "D" &=
                help "Just dump parser output (haskell or json), default: no"
              }
          &= summary "Advent of Code solutions by Zouppen"

main :: IO ()
main = do
  Args{..} <- cmdArgs argsDef
  case M.lookup day tontut of
    Just (Tonttu t) -> t input parts
    Nothing         -> fail $ "Not a valid day. Valid days: " <> dayList
      where dayList = intercalate ", " $ map show $ M.keys tontut
