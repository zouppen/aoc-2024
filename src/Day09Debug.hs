-- |Day 09 debugging tools because I was, once again, frustrated.
module Day09Debug where

import qualified Data.IntMap as IM

renderDisk :: Show a => IM.IntMap a -> String
renderDisk = concat . renderDisk' 0 . IM.toList

renderDisk' :: Show a => Int -> [(Int, a)] -> [String]
renderDisk' _ [] = []
renderDisk' pos xxs@((posNext, fileId):xs) =
  let iter = renderDisk' (pos+1)
  in if pos == posNext
     then show fileId : iter xs
     else "." : iter xxs

projFromList :: (a -> IM.Key) -> [a] -> IM.IntMap a
projFromList p = IM.fromListWith dupErr . map (\f -> (p f, f))
  where dupErr = error "Collision detected while projecting"

projMap :: (a -> IM.Key) -> IM.IntMap a -> IM.IntMap a
projMap p = projFromList p . IM.elems
