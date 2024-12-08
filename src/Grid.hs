{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Grid ( Grid(..)
            , bounds
            , gridParser
            ) where

import Control.Applicative
import Control.Monad (unless)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Aeson as A
import GHC.Generics
import Text.Printf

data Grid a = Grid { stuff  :: a
                   , rows   :: !Int
                   , cols   :: !Int
                   , trail  :: !Int
                   } deriving (Show, Generic)

instance (A.ToJSON a) => A.ToJSON (Grid a)

bounds :: Grid a -> (Int, Int) -> Bool
bounds Grid{..} (x, y) = x >= 0 && x < cols &&
                         y >= 0 && y < rows

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
