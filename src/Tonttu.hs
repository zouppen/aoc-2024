{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Tonttu (Tonttu(..), parseTextFile, t) where

import Data.String (fromString)
import Data.Foldable (traverse_)
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
  case (json, null parts) of
    (False, True)  -> traverse_ (runner input) solvers
    (False, False) -> traverse_ (finder input) parts
    (True,  True)  -> jsonify $ A.object $ "input" A..= input : map (jsonRunner input) solvers
    (True,  False) -> traverse (jsonFinder input) parts >>= jsonify . A.object
  where runner input (solverName, solver) = do
          putStr $ "Running " <> solverName <> "... "
          case solver of
            ShowSolver f   -> print $ f input
            StringSolver f -> putStrLn $ f input
        jsonRunner input (solverName, solver) = case solver of
          ShowSolver f    -> fromString solverName A..= f input
          StringSolver f  -> fromString solverName A..= f input
        jsonFinder input "input" = pure $ "input" A..= input
        jsonFinder input part = case M.lookup part m of
          Nothing -> fail $ "Part name unknown. Should be one of: " <> partList
          Just a  -> pure $ jsonRunner input (part, a)
        finder input "input" = do
          putStr "Parser output: "
          print input
        finder input part = case M.lookup part m of
          Nothing -> fail $ "Part name unknown. Should be one of: " <> partList
          Just a  -> runner input (part, a)
        m = M.fromList solvers
        partList = intercalate ", " $ "input" : map fst solvers
        jsonify :: A.ToJSON a => a -> IO ()
        jsonify a = B.hPut stdout $ A.encode a <> "\n"

t :: (A.ToJSON a, A.ToJSON b, Show a, Show b) => k -> Day a b -> M.Map k Tonttu
t i task = M.singleton i $ Tonttu $ tonttu task
