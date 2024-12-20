-- |Functions which are useful almost every day.
module AocTools.Everyday
  ( bsearch
  , minFold
  ) where

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
