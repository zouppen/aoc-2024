{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Grid ( Grid(..)
            , Coord
            , bounds
            , gridCoord
            , gridParser
            , renderGrid
            ) where

import Control.Applicative
import Control.Monad (unless)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import GHC.Generics
import Text.Printf

-- |Coordinate pair (x, y)
type Coord = (Int, Int)

data Grid a = Grid { stuff  :: a
                   , rows   :: !Int
                   , cols   :: !Int
                   , trail  :: !Int
                   } deriving (Show, Eq, Generic)

instance (A.ToJSON a) => A.ToJSON (Grid a)

bounds :: Grid a -> Coord -> Bool
bounds Grid{..} (x, y) = x >= 0 && x < cols &&
                         y >= 0 && y < rows

-- |Get coordinate in format (x,y) at parse time. Later gives final
-- coordinate where parsing ended which might not be what you've
-- expected.
gridCoord :: Grid a -> Coord
gridCoord g = (trail g, rows g)

gridParser :: (Grid a -> Parser a) -> a -> Parser (Grid a)
gridParser p emptyA = cells' $ Grid emptyA 0 0 0
  where cells' g = (endOfInput >> validate g >> pure g) <|>
                   (cell p g >>= cells')

cell :: (Grid a -> Parser a) -> Grid a -> Parser (Grid a)
cell p g = (endOfLine >> nextRow g) <|>
           ((wrap <$> p g) >>= nextCol)
  where wrap a = g{stuff = a}

nextCol :: Applicative f => Grid a -> f (Grid a)
nextCol Grid{..} = pure Grid{trail = trail + 1, ..}

nextRow :: MonadFail f => Grid a -> f (Grid a)
nextRow Grid{..} = if rows == 0 || cols == trail
                   then pure Grid{ rows = rows + 1
                                 , cols = trail
                                 , trail = 0
                                 , ..
                                 }
                   else fail $ printf "Row %d length %d is uneven" rows trail

validate :: MonadFail f => Grid a -> f ()
validate Grid{..} = unless (trail == 0) $ fail $
  printf "Trailing row %d length %d is uneven" rows trail

renderGrid :: Grid a -> (Coord -> Char) -> B.ByteString
renderGrid Grid{..} charFunc = fst $ B.unfoldrN size f (0,0)
  where size = (cols+1)*rows
        f (x,y) = Just $ if x == cols
                             then ('\n', (0, y+1))
                             else (charFunc (x,y), (x+1, y))
