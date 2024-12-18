-- |Functions which are useful almost every day.
module AocTools.Everyday
  ( bsearch
  ) where

import Data.Bits

bsearch :: (Ord a, Bits a, Num a) => (a -> Bool) -> a -> a -> a
bsearch test l r = if l < r
                   then let m = (l+r) .>>. 1
                        in if test m
                           then bsearch test l m
                           else bsearch test (m+1) r
                   else l
