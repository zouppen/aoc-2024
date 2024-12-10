{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Tonttu ( Runner(..)
              , Tonttu(..)
              , parseBinFile
              , t
              ) where

import Control.Concurrent.STM.TMVar
import Control.Concurrent (forkIO)
import Control.DeepSeq (NFData(..), deepseq)
import Control.Monad.STM
import Data.Aeson (Value, ToJSON, toJSON)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.ByteString (Parser, eitherResult, parseWith)
import qualified Data.ByteString as B
import System.Clock
import System.IO (IOMode(..), withFile)

import Day

data Tonttu = Tonttu { plainRunner :: FilePath -> [Text] -> IO [Runner String]
                     , jsonRunner  :: FilePath -> [Text] -> IO [Runner Value]
                     }

data Runner a = Runner { infoText  :: Text
                       , startTime :: TimeSpec
                       , resultAct :: STM (TimeSpec, a)
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

-- |Type which carries some useful functions and variables from the
-- more generic runnerWrap to runPlain and runJson. Type variable
-- meanings: i = input, o = output, s showed type (String or Value)
data CommonVars i o s = CommonVars
  { lookupPart :: Text -> IO (Text, Solver i o)
  , inputAct   :: STM i
  , toRunner   :: Text -> IO s -> IO (Runner s)
  }

runnerWrap :: NFData s => (Day i o -> CommonVars i o s -> [Text] -> IO [Runner s]) -> Day i o -> FilePath -> [Text] -> IO [Runner s]
runnerWrap act day@Day{..} file parts = do
  startTime <- getTime MonotonicCoarse
  inputAct <- bgRun $ parseBinFile parser file
  let m = M.fromList solvers
      partList = T.intercalate ", " $ "input" : map fst solvers
      lookupPart part = case M.lookup part m of
          Nothing -> fail $ T.unpack $ "Part name unknown. Should be one of: " <> partList
          Just a  -> pure (part, a)
      toRunner infoText calc = do
        resultAct <- bgRun $ do
          -- Do the operation
          result <- calc
          -- Force evaluation and get time
          now <- result `deepseq` getTime MonotonicCoarse
          pure (now, result)
        pure Runner{..}
  act day CommonVars{..} parts

runPlain :: (Show i, Show o) => Day i o -> CommonVars i o String -> [Text] -> IO [Runner String]
runPlain Day{..} CommonVars{..} parts = do
  case null parts of
    True  -> traverse runner solvers
    False -> traverse finder parts
  where runner (name, solver) = do
          toRunner name $ do
            input <- atomically inputAct
            pure $ case solver of
              ShowSolver f   -> show $ f input
              StringSolver f -> f input
        finder "input" =
          toRunner "parser only" $ show <$> atomically inputAct
        finder part = lookupPart part >>= runner

runJson :: (ToJSON i, ToJSON o) => Day i o -> CommonVars i o Value -> [Text] -> IO [Runner Value]
runJson Day{..} CommonVars{..} parts = do
  case null parts of
    True  -> traverse runner solvers
    False -> traverse finder parts
  where runner (name, solver) = do
          toRunner name $ do
            input <- atomically inputAct
            pure $ case solver of
              ShowSolver f    -> toJSON $ f input
              StringSolver f  -> toJSON $ f input
        finder "input" =
          toRunner "input" $ toJSON <$> atomically inputAct
        finder part = lookupPart part >>= runner

t :: (ToJSON a, ToJSON b, Show a, Show b) => k -> Day a b -> M.Map k Tonttu
t i task = M.singleton i $ Tonttu { plainRunner = runnerWrap runPlain task
                                  , jsonRunner = runnerWrap runJson task
                                  }
