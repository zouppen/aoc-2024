module Tontut (tontut) where

import Tonttu (Tonttu, t)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12

tontut :: [Maybe Tonttu]
tontut = [ t Day01.task
         , t Day02.task
         , t Day03.task
         , t Day04.task
         , t Day05.task
         , t Day06.task
         , t Day07.task
         , t Day08.task
         , t Day09.task
         , t Day10.task
         , t Day11.task
         , t Day12.task
         ]
