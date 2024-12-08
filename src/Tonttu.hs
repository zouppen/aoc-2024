{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Tonttu (Tonttu(..), parseBinFile, t) where

import Data.Foldable (traverse_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.ByteString (Parser, eitherResult, parseWith)
import qualified Data.ByteString as B
import System.IO (IOMode(..), withFile, stdout)

import Day

newtype Tonttu = Tonttu (Bool -> FilePath -> [T.Text] -> IO ())

parseBinFile :: Parser a -> FilePath -> IO a
parseBinFile p f = withFile f ReadMode $ \h -> do
  out <- parseWith (B.hGetSome h 0x4000) p mempty
  either fail pure $ eitherResult out

tonttu :: (A.ToJSON a, A.ToJSON b, Show a, Show b) => Day a b -> Bool -> FilePath -> [T.Text] -> IO ()
tonttu Day{..} json file parts = do
  input <- parseBinFile parser file
  case (json, null parts) of
    (False, True)  -> traverse_ (runner input) solvers
    (False, False) -> traverse_ (finder input) parts
    (True,  True)  -> jsonify $ A.object $ "input" A..= input : map (jsonRunner input) solvers
    (True,  False) -> traverse (jsonFinder input) parts >>= jsonify . A.object
  where runner input (solverName, solver) = do
          T.putStr $ "Running " <> solverName <> "... "
          case solver of
            ShowSolver f   -> print $ f input
            StringSolver f -> putStrLn $ f input
        jsonRunner input (solverName, solver) = case solver of
          ShowSolver f    -> A.fromText solverName A..= f input
          StringSolver f  -> A.fromText solverName A..= f input
        jsonFinder input "input" = pure $ "input" A..= input
        jsonFinder input part = case M.lookup part m of
          Nothing -> fail $ T.unpack $ "Part name unknown. Should be one of: " <> partList
          Just a  -> pure $ jsonRunner input (part, a)
        finder input "input" = do
          T.putStr "Parser output: "
          print input
        finder input part = case M.lookup part m of
          Nothing -> fail $ T.unpack $ "Part name unknown. Should be one of: " <> partList
          Just a  -> runner input (part, a)
        m = M.fromList solvers
        partList = T.intercalate ", " $ "input" : map fst solvers
        jsonify :: A.ToJSON a => a -> IO ()
        jsonify a = BL.hPut stdout $ A.encode a <> "\n"

t :: (A.ToJSON a, A.ToJSON b, Show a, Show b) => k -> Day a b -> M.Map k Tonttu
t i task = M.singleton i $ Tonttu $ tonttu task
