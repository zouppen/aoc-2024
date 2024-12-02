{-# LANGUAGE RecordWildCards #-}
module Tonttu (Tonttu(..), t) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import Data.Attoparsec.Text (Parser, eitherResult, parseWith)
import Data.Text.IO (hGetChunk)
import Data.List (intercalate)
import System.IO (IOMode(..), withFile, stdout, hPrint)

import Day

newtype Tonttu = Tonttu (FilePath -> [String] -> IO ())

parseTextFile :: Parser a -> FilePath -> IO a
parseTextFile p f = withFile f ReadMode $ \h -> do
  out <- parseWith (hGetChunk h) p mempty
  either fail pure $ eitherResult out

tonttu :: (A.ToJSON a, Show a, Show b) => Day a b -> FilePath -> [String] -> IO ()
tonttu Day{..} file parts = do
  input <- parseTextFile parser file
  _ <- if null parts
    then traverse (runner input) solvers
    else traverse (finder input) parts
  pure ()
  where runner input (solverName, solver) = do
          putStr $ "Running " <> solverName <> "... "
          case solver of
            ShowSolver f     -> print $ f input
            StringSolver f   -> putStrLn $ f input
            HandleSolver act -> putStrLn "" >> act stdout input
        m = M.fromList $ solvers <> extras
        finder input part = case M.lookup part m of
          Nothing -> fail $ "Part name unknown. Should be one of: " <> partList
          Just a  -> runner input (part, a)
        partList = intercalate ", " $ map fst (solvers <> extras)
        extras = [("haskell", HandleSolver hPrint)
                 ,("json", HandleSolver (\h a -> B.hPut h $ A.encode a))
                 ]

t :: (A.ToJSON a, Show a, Show b) => k -> Day a b -> M.Map k Tonttu
t i task = M.singleton i $ Tonttu $ tonttu task
