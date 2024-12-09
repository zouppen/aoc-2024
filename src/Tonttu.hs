{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Tonttu ( Runner(..)
              , Tonttu(..)
              , parseBinFile
              , t
              ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Attoparsec.ByteString (Parser, eitherResult, parseWith)
import qualified Data.ByteString as B
import System.IO (IOMode(..), withFile)
import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent (forkIO)

import Day

data Runner a = Runner { infoText :: T.Text
                       , result   :: STM a
                       }

data Tonttu = Tonttu { plainRunner :: FilePath -> [T.Text] -> IO [Runner String]
                     , jsonRunner  :: FilePath -> [T.Text] -> IO A.Value
                     }

parseBinFile :: Parser a -> FilePath -> IO a
parseBinFile p f = withFile f ReadMode $ \h -> do
  out <- parseWith (B.hGetSome h 0x4000) p mempty
  either fail pure $ eitherResult out

bgRun :: IO a -> IO (STM a)
bgRun act = do
  var <- newEmptyTMVarIO
  _ <- forkIO $ do
    x <- act
    atomically $ putTMVar var x
  pure $ readTMVar var

runPlain :: (A.ToJSON a, A.ToJSON b, Show a, Show b) => Day a b -> FilePath -> [T.Text] -> IO [Runner String]
runPlain Day{..} file parts = do
  inputAct <- bgRun $ parseBinFile parser file
  case null parts of
    True  -> traverse (runner inputAct) solvers
    False -> traverse (finder inputAct) parts
  where runner inputAct (solverName, solver) = do
          let infoText = "Running " <> solverName <> "... "
          result <- bgRun $ do
            input <- atomically inputAct
            pure $ case solver of
              ShowSolver f   -> show $ f input
              StringSolver f -> f input
          pure Runner{..}
        finder inputAct "input" = do
          let infoText = "Parser output: "
          result <- bgRun $ show <$> atomically inputAct
          pure Runner{..}
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
