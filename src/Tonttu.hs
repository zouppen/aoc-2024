{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Tonttu ( Runner(..)
              , Tonttu(..)
              , parseBinFile
              , t
              ) where

import Control.Concurrent.STM.TMVar
import Control.Concurrent (forkIO)
import Control.DeepSeq (NFData(..), deepseq)
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Attoparsec.ByteString (Parser, eitherResult, parseWith)
import qualified Data.ByteString as B
import System.IO (IOMode(..), withFile)
import Control.Monad.STM

import Day

data Runner a = Runner { infoText :: T.Text
                       , result   :: STM a
                       }

data Tonttu = Tonttu { plainRunner :: FilePath -> [T.Text] -> IO [Runner String]
                     , jsonRunner  :: FilePath -> [T.Text] -> IO [Runner A.Value]
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

bgRunDeep :: NFData a => IO a -> IO (STM a)
bgRunDeep act = do
  var <- newEmptyTMVarIO
  _ <- forkIO $ do
    x <- act
    x `deepseq` (atomically $ putTMVar var x)
  pure $ readTMVar var

runPlain :: (A.ToJSON a, A.ToJSON b, Show a, Show b) => Day a b -> FilePath -> [T.Text] -> IO [Runner String]
runPlain day@Day{..} file parts = do
  inputAct <- bgParse file
  case null parts of
    True  -> traverse (runner inputAct) solvers
    False -> traverse (finder inputAct) parts
  where runner inputAct (solverName, solver) = do
          let infoText = "Running " <> solverName <> "... "
          result <- bgRunDeep $ do
            input <- atomically inputAct
            pure $ case solver of
              ShowSolver f   -> show $ f input
              StringSolver f -> f input
          pure Runner{..}
        finder inputAct "input" = do
          let infoText = "Parser output: "
          result <- bgRun $ show <$> atomically inputAct
          pure Runner{..}
        finder inputAct part = withPart part $ runner inputAct
        CommonVars{..} = commonVars day

runJson :: (A.ToJSON a, A.ToJSON b, Show a, Show b) => Day a b -> FilePath -> [T.Text] -> IO [Runner A.Value]
runJson day@Day{..} file parts = do
  inputAct <- bgParse file
  case null parts of
    True  -> traverse (runner inputAct) solvers
    False -> traverse (finder inputAct) parts
  where runner inputAct (solverName, solver) = do
          result <- bgRunDeep $ do
            input <- atomically inputAct
            pure $ case solver of
              ShowSolver f    -> A.toJSON $ f input
              StringSolver f  -> A.toJSON $ f input
          pure Runner{infoText = solverName, ..}
        finder inputAct "input" = do
          result <- bgRun $ A.toJSON <$> atomically inputAct
          pure Runner{infoText = "input", ..}
        finder inputAct part = withPart part $ runner inputAct
        CommonVars{..} = commonVars day

data CommonVars a b c = CommonVars
  { withPart :: T.Text
             -> ((T.Text, Solver a b) -> IO (Runner c))
             -> IO (Runner c)
  , bgParse :: FilePath -> IO (STM a)
  }

commonVars :: Day a b -> CommonVars a b c
commonVars Day{..} = CommonVars{..}
  where partList     = T.intercalate ", " $ "input" : map fst solvers
        withPart part act = case M.lookup part m of
          Nothing -> fail $ T.unpack $ "Part name unknown. Should be one of: " <> partList
          Just a  -> act (part, a)
        m            = M.fromList solvers
        bgParse file = bgRun $ parseBinFile parser file

t :: (A.ToJSON a, A.ToJSON b, Show a, Show b) => k -> Day a b -> M.Map k Tonttu
t i task = M.singleton i $ Tonttu { plainRunner = runPlain task
                                  , jsonRunner = runJson task
                                  }
