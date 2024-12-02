{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Tonttu (Tonttu(..), parseTextFile, t) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import Data.Attoparsec.Text (Parser, eitherResult, parseWith)
import Data.Text.IO (hGetChunk)
import Data.List (intercalate)
import System.IO (IOMode(..), withFile, stdout)

import Day

newtype Tonttu = Tonttu (Bool -> FilePath -> [String] -> IO ())

parseTextFile :: Parser a -> FilePath -> IO a
parseTextFile p f = withFile f ReadMode $ \h -> do
  out <- parseWith (hGetChunk h) p mempty
  either fail pure $ eitherResult out

tonttu :: (A.ToJSON a, A.ToJSON b, Show a, Show b) => Day a b -> Bool -> FilePath -> [String] -> IO ()
tonttu Day{..} json file parts = do
  input <- parseTextFile parser file
  _ <- if null parts
    then traverse (runner input) solvers
    else traverse (finder input) parts
  pure ()
  where runner input (solverName, solver) = do
          putStr $ "Running " <> solverName <> "... "
          case (json, solver) of
            (False, ShowSolver f)   -> print $ f input
            (False, StringSolver f) -> putStrLn $ f input
            (True, ShowSolver f)    -> jsonify $ f input
            (True, StringSolver f)  -> jsonify $ f input
        m = M.fromList solvers
        finder input "input" = do
          putStr "Parser output: "
          if json
            then jsonify input
            else print input
        finder input part = case M.lookup part m of
          Nothing -> fail $ "Part name unknown. Should be one of: " <> partList
          Just a  -> runner input (part, a)
        partList = intercalate ", " $ "input" : map fst solvers
        jsonify :: A.ToJSON a => a -> IO ()
        jsonify a = B.hPut stdout $ "\n" <> A.encode a <> "\n"

t :: (A.ToJSON a, A.ToJSON b, Show a, Show b) => k -> Day a b -> M.Map k Tonttu
t i task = M.singleton i $ Tonttu $ tonttu task
