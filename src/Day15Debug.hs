{-# LANGUAGE RecordWildCards #-}
-- |Day 15 debugging tools
module Day15Debug where

import qualified Data.Map as M

import Day15

renderInput :: Input -> String
renderInput Input{..} =
  [ if x == maxX+1
    then '\n'
    else if robotPos == (x, y)
         then '@'
         else case (M.lookup (x, y) arena) of
                Just Wall    -> '#'
                Just Box     -> 'O'
                Just BigBoxL -> '['
                Just BigBoxR -> ']'
                Nothing -> '.'
  | y <- [0..maxY]
  , x <- [0..maxX+1]
  ]
  where ((maxX, maxY),_) = M.findMax arena
