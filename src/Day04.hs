{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Day04 where

import qualified Data.Aeson as A
import Data.Attoparsec.ByteString hiding (take)
import qualified Data.ByteString.Char8 as B
import Data.Word (Word8)
import GHC.Generics

import Day

task :: Day Kattila Int
task = Day { parser  = kattila
           , solvers = [ part 1 $ length . filter manteli . joulupuuro
                       , part 2 $ length . raportoi
                       ]
           }

-- Types

data Kattila = Kattila { input  :: B.ByteString
                       , bounds :: Bounds
                       } deriving (Show, Generic)

instance A.ToJSON Kattila where
  toJSON Kattila{..} = A.object ["input" A..= B.unpack input, "bounds" A..= bounds]

data Bounds = Bounds { rows  :: !Int
                     , cols  :: !Int
                     , trail :: !Int
                     } deriving (Show, Generic)

instance A.ToJSON Bounds

data Poro = Poro { pos    :: (Int, Int)
                 , expect :: Char
                 } deriving (Show)

type Tokka = [Poro]

kattila :: Parser Kattila
kattila = uncurry Kattila <$> runScanner (Bounds 0 0 0) puurokauha

-- For unknown reason there's no runScanner for Char8, so we can't
-- have newline as char literal.

puurokauha :: Bounds -> Word8 -> Maybe Bounds
puurokauha Bounds{..} c = if c == 0x0a -- newline
                         then if rows == 0 || cols == trail
                              then Just $ Bounds (rows+1) trail 0
                              else Nothing -- not rectangular
                         else if inClass "XMAS." c
                              then Just $ Bounds rows cols (trail+1)
                              else Nothing -- not valid input

toYX :: Bounds -> Int -> (Int, Int)
toYX Bounds{..} i = quotRem i (cols+1)

fromYX :: Bounds -> (Int, Int) -> Maybe Int
fromYX Bounds{..} (y, x) | x < 0 || x >= cols = Nothing
                         | y < 0 || y >= rows = Nothing
                         | otherwise          = Just $ (cols+1) * y + x

streak :: Kattila -> (Int, Int) -> Int -> String
streak Kattila{..} inc i = map (B.index input) $ streak' bounds inc $ toYX bounds i

streak' :: Bounds -> (Int, Int) -> (Int, Int) -> [Int]
streak' st inc@(iy, ix) cur@(y, x) = case fromYX st cur of
  Nothing -> [] -- Out of bounds
  Just i  -> i : streak' st inc (y+iy, x+ix)

directions :: [(Int, Int)]
directions = [( 0,  1) -- From left to right
             ,( 1,  0) -- From up to down
             ,( 0, -1) -- From right to left
             ,(-1,  0) -- From down to up
             ,( 1,  1) -- From top left to bottom right
             ,(-1, -1) -- From bottom right to top left
             ,( 1, -1) -- From top right to bottom left
             ,(-1,  1) -- From bottom left to top right
             ]

joulupuuro :: Kattila -> [String]
joulupuuro stuff = [ streak stuff dir i
                   | dir <- directions
                   , i <- [0..B.length (input stuff) -1]
                   ]

manteli :: String -> Bool
manteli s = "XMAS" == take 4 s

ahkiot :: [String]
ahkiot = [ "M S\
           \ A \
           \M S"
         , "S S\
           \ A \
           \M M"
         , "M M\
           \ A \
           \S S"
         , "S M\
           \ A \
           \S M"
         ]

toTokka :: String -> Tokka
toTokka xs = filter ((' '/=).expect) $ zipWith Poro [(y, x) | y <- [-1..1] , x <- [-1..1]] xs

rekiretki :: [Tokka]
rekiretki = map toTokka ahkiot

urki :: Kattila -> Int -> Poro -> Bool
urki Kattila{..} i Poro{..} = case fromYX bounds (y+oy, x+ox) of
  Nothing -> False
  Just oi -> expect == B.index input oi
  where (y, x)   = toYX bounds i
        (oy, ox) = pos

kurki :: Kattila -> Int -> Tokka -> Bool
kurki kama i tokka = all (urki kama i) tokka

vakoile :: Kattila -> Int -> Bool
vakoile kama i = any (kurki kama i) rekiretki

raportoi :: Kattila -> [Int]
raportoi kama = filter (vakoile kama) [0..(B.length $ input kama)]
