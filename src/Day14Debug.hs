{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Day14Debug where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Set as S
import Data.Foldable (traverse_)
import System.IO

import Grid
import Day14

renderVideo :: Grid [Robot] -> Handle -> IO ()
renderVideo g h = write $ zipWith makeFrame [0..] oneCycle
  where write = traverse_ $ B.hPut h
        oneCycle = uncycle $ iterate generation g

makeFrame :: Int -> Grid [Robot] -> B.ByteString
makeFrame genId g = B.pack $ frameLine <> pixels
  where
    set = S.fromList $ map robotToCoords $ stuff g
    robotToCoords Robot{..} = (posX, posY)
    frameLine = map bitToPixel [0..cols g-1]
    pixels = [ if S.member (x,y) set then 0xff else 0x00
             | y <- [0..rows g - 1]
             , x <- [0..cols g - 1]
             ]
    -- Not the Gray Coding you learned in the uni :-D
    bitToPixel i = if testBit genId i
                   then 0x80
                   else 0x40
