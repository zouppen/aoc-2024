{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Day15 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.List (unfoldr)
import qualified Data.Map as M

import Day
import Grid
import Wastl (direction, ignoreNewline)

task :: Day Input Int
task = Day { parser  = everything
           , solvers = [ part 1 $ score . last . iterateJust generation
                       , part 2 $ score . last . iterateJust generation . enlarge
                       ]
           }

data Input = Input { arena      :: M.Map Coord Obj
                   , robotPos   :: Coord
                   , directions :: [Coord]
                   } deriving (Show, Generic)

data Obj = Wall | Box | BigBoxL | BigBoxR deriving (Show, Generic)

data Arena = Arena { robots  :: [Coord]
                   , arena'  :: M.Map Coord Obj
                   } deriving (Show, Generic)

instance ToJSON Obj
instance ToJSON Input

-- Parsing

everything :: Parser Input
everything = do
  Grid{..} <- gridParser cell (Arena mempty mempty)
  directions <- many $ ignoreNewline direction
  endOfLine
  endOfInput
  robotPos <- case robots stuff of
    [a] -> pure a
    _   -> fail "Incorrect amount of robots, expecting only one"
  pure Input{arena = arena' stuff, ..}

cell :: Grid Arena -> Parser Arena
cell g = anyChar >>= \c -> case c of
  'O' -> pure $ item Box
  '#' -> pure $ item Wall
  '@' -> pure old{robots = pos : robots old}
  '.' -> pure old -- Empty cells are not populated
  _   -> empty
  where old = stuff g
        pos = gridCoord g
        item a = old{ arena' = M.insert pos a $ arena' old }

-- Sokoban part begins

iterateJust :: (a -> Maybe a) -> a -> [a]
iterateJust f = unfoldr (fmap wrap . f)
  where wrap a = (a, a)

generation :: Input -> Maybe Input
generation old@Input{..} = case directions of
  (dir:rest) -> let tryPos = addCoord robotPos dir
                in case push tryPos dir arena of
    Just newArena -> Just Input{ directions = rest
                               , robotPos = tryPos
                               , arena = newArena
                               , ..
                               }
    Nothing -> Just old{directions = rest} -- Nothing moved
  [] -> Nothing -- End of input

-- |Push boxes at given direction or fail to do so
push :: Coord -> Coord -> M.Map Coord Obj -> Maybe (M.Map Coord Obj)
push pos d m = case (m M.!? pos, vertical) of
  (Nothing, _)         -> Just m  -- Free space, return self
  (Just Wall, _)       -> Nothing -- None shall pass
  -- Box moves. In case of big boxes, both sides must be pushed
  (Just BigBoxL, True) -> tryPush pos m >>= tryPush (addCoord pos (1,0))
  (Just BigBoxR, True) -> tryPush pos m >>= tryPush (addCoord pos (-1,0))
  (Just _, _)          -> tryPush pos m -- Horizontal moves or small boxes
  where vertical = snd d /= 0
        tryPush from cur = let to = addCoord from d
                           in move from to <$> push to d cur

move :: (Show k, Ord k) => k -> k -> M.Map k a -> M.Map k a
move from to m = case (M.lookup from m, M.member to m) of
  (Just a, False) -> M.insert to a $ M.delete from m
  _ -> error "Illegal move "

gps :: (Coord, Obj) -> Int
gps ((x, y), o) = case o of
  Wall    -> 0
  BigBoxR -> 0 -- We calculate from the left part
  _       -> x + 100*y

score :: Input -> Int
score Input{..} = sum $ map gps $ M.toList arena

-- Part 2 magic enlargement

enlarge :: Input -> Input
enlarge o = o{ arena = M.fromList $ concatMap enlargeCell $ M.toList $ arena o
             , robotPos = let f (x, y) = (2*x, y)
                          in f (robotPos o)
             }

enlargeCell :: (Coord, Obj) -> [(Coord, Obj)]
enlargeCell ((x, y), o) = case o of
  Box  -> zip sides [BigBoxL, BigBoxR]
  Wall -> zip sides [Wall, Wall]
  _    -> error "Can't enlarge further"
  where sides = [(2*x, y), (2*x+1, y)]
