{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Tonttu (Tonttu(..), parseBinFile, t) where

import Data.Foldable (traverse_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.ByteString (Parser, eitherResult, parseWith)
import qualified Data.ByteString as B
import System.IO (IOMode(..), withFile, stdout, hFlush)

import Day

data Tonttu = Tonttu { plainRunner :: FilePath -> [T.Text] -> IO ()
                     , jsonRunner  :: FilePath -> [T.Text] -> IO A.Value
                     }

parseBinFile :: Parser a -> FilePath -> IO a
parseBinFile p f = withFile f ReadMode $ \h -> do
  out <- parseWith (B.hGetSome h 0x4000) p mempty
  either fail pure $ eitherResult out

runPlain :: (A.ToJSON a, A.ToJSON b, Show a, Show b) => Day a b -> FilePath -> [T.Text] -> IO ()
runPlain Day{..} file parts = do
  input <- parseBinFile parser file
  case null parts of
    True  -> traverse_ (runner input) solvers
    False -> traverse_ (finder input) parts
  where runner input (solverName, solver) = do
          T.putStr $ "Running " <> solverName <> "... "
          hFlush stdout
          case solver of
            ShowSolver f   -> print $ f input
            StringSolver f -> putStrLn $ f input
        finder input "input" = do
          T.putStr "Parser output: "
          print input
        finder input part = case M.lookup part m of
          Nothing -> fail $ T.unpack $ "Part name unknown. Should be one of: " <> partList
          Just a  -> runner input (part, a)
        m = M.fromList solvers
        partList = T.intercalate ", " $ "input" : map fst solvers

runJson :: (A.ToJSON a, A.ToJSON b, Show a, Show b) => Day a b -> FilePath -> [T.Text] -> IO A.Value
runJson Day{..} file parts = do
  input <- parseBinFile parser file
  case null parts of
    True  -> jsonify $ A.object $ "input" A..= input : map (runner input) solvers
    False -> traverse (finder input) parts >>= jsonify . A.object
  where runner input (solverName, solver) = case solver of
          ShowSolver f    -> A.fromText solverName A..= f input
          StringSolver f  -> A.fromText solverName A..= f input
        finder input "input" = pure $ "input" A..= input
        finder input part = case M.lookup part m of
          Nothing -> fail $ T.unpack $ "Part name unknown. Should be one of: " <> partList
          Just a  -> pure $ runner input (part, a)
        m = M.fromList solvers
        partList = T.intercalate ", " $ "input" : map fst solvers
        jsonify = pure . A.toJSON

t :: (A.ToJSON a, A.ToJSON b, Show a, Show b) => k -> Day a b -> M.Map k Tonttu
t i task = M.singleton i $ Tonttu { plainRunner = runPlain task
                                  , jsonRunner = runJson task
                                  }
