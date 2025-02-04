-- |Functions which are useful almost every day.
module AocTools.Everyday
  ( bsearch
  , expectOne
  , diamond
  , liftMaybe
  , minFold
  ) where

import Control.Applicative
import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Bits

bsearch :: (Ord a, Bits a, Num a) => (a -> Bool) -> a -> a -> a
bsearch test l r = if l < r
                   then let m = (l+r) .>>. 1
                        in if test m
                           then bsearch test l m
                           else bsearch test (m+1) r
                   else l

-- |Collects all smallest values (based on given comparator. Useful
-- with foldrWithKey from Data.Map but works on unsorted lists, too.
minFold :: (a -> a -> Ordering) -> k -> a -> (a, [k]) -> (a, [k])
minFold cmp k b (minVal, xs) = case cmp minVal b of
  LT -> (minVal, xs)   -- Ignore value
  GT -> (b, [k])       -- New minimum, throw old away
  EQ -> (minVal, k:xs) -- Collect tie values

-- |Produces a possibly hollow diamond around given coordinate. Note
-- that the hollow radius is zero based, so with radius 1 and hollow 0
-- you'll get nearest neighbours and the node itself. With radius 1
-- and hollow 1, you'll get nearest neighbours only. So the wall
-- thicknes is radius-hollow+1.
diamond :: (Enum a, Ord a, Num a) => a -> a -> (a, a) -> [(a, a)]
diamond radius hollow (xOrigin, yOrigin) = do
  yDelta <- [-radius..radius]
  let xOff = radius - abs yDelta
  xDelta <- [-xOff..xOff]
  guard $ abs xDelta + abs yDelta >= hollow
  pure (xOrigin + xDelta, yOrigin + yDelta)

-- |Expects a singleton value. Give a more descriptive error message
-- if not so. Avoids iterating over an infinite list if one is given.
expectOne :: Foldable t => String -> t a -> a
expectOne name as = case toList as of
  [a] -> a
  []  -> f "none"
  _   -> f "many"
  where f m = error $ "Expecting a singleton " <> name <> " but there are " <> m

-- | Lift a maybe to an Alternative. Copied from Agda.
liftMaybe :: Alternative f => Maybe a -> f a
liftMaybe = maybe empty pure
