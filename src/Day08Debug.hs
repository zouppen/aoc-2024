{-# LANGUAGE OverloadedStrings #-}
-- |Day 08 debugging tools because I was frustrated.
module Day08Debug where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Set as S
import Grid

import Day08

tryPart2 :: Grid Antennas -> Char -> IO ()
tryPart2 grid c = B.putStrLn $ case M.lookup c $ getAntennas grid of
  Nothing     -> "Tower not found"
  Just towers -> renderGrid grid $ proj c towers $ antinodeSet (antinodesRepeat grid) grid towers

proj :: Char -> Coords -> Coords -> (Int, Int) -> Char
proj c towers antinodes coord = case (S.member coord towers, S.member coord antinodes) of
  (True,  True ) -> '@' -- Overlapping
  (True,  False) -> c
  (False, True ) -> '#'
  (False, False) -> '.'
