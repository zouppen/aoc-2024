module Tontut (Tonttu(..), tontut) where

import Tonttu
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import Data.Map.Strict (Map)

tontut :: Map Int Tonttu
tontut = t 1 Day01.task <>
         t 2 Day02.task <>
         t 3 Day03.task <>
         t 4 Day04.task <>
         t 5 Day05.task <>
         t 6 Day06.task <>
         t 7 Day07.task <>
         mempty
