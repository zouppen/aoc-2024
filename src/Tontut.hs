module Tontut (Tonttu(..), tontut) where

import Tonttu
import qualified Day01
import qualified Day02
import Data.Map.Strict (Map)

tontut :: Map Int Tonttu
tontut = t 1 Day01.task <>
         t 2 Day02.task
