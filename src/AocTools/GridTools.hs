{-# LANGUAGE RecordWildCards #-}
module AocTools.GridTools
  ( BoundInfo(..)
  , getBoundsXY
  ) where

data BoundInfo a = BoundInfo { xMin :: a
                             , xMax :: a
                             , yMin :: a
                             , yMax :: a
                             } deriving (Show)

findBounds :: Ord a => BoundInfo a -> (a, a) -> BoundInfo a
findBounds BoundInfo{..} (x,y) = BoundInfo{ xMin = min xMin x
                                          , yMin = min yMin y
                                          , xMax = max xMax x
                                          , yMax = max yMax y
                                          }

boundSeed :: (a, a) -> BoundInfo a
boundSeed (x, y) = BoundInfo{ xMin = x, yMin = y
                            , xMax = x, yMax = y
                            }

-- Minimum is is only fast on sets and maps and similar. It can be any
-- item but then foldable property is lost.

getBoundsXY :: (Foldable t, Ord a) => t (a, a) -> BoundInfo a
getBoundsXY xs = foldl findBounds (boundSeed $ minimum xs) xs
