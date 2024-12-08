{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Day06 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Set as S
import qualified Data.Aeson as A
import Control.Parallel.Strategies
import GHC.Generics
import Grid

import Day

task :: Day (Grid Objects) Int
task = Day { parser = gridParser cell $ Objects mempty mempty
           , solvers = [("part1", ShowSolver $ length . S.fromList . map toLocation . \a -> travel a $ singlePatrol a)
                       ,("part2", ShowSolver $ length . filter id .
                          withStrategy (parListChunk 12 rseq) .
                          map gridHasLoop . addObsts)
                       ]
           }

data Direction = ToLeft
               | ToRight
               | ToUp
               | ToDown
               deriving (Show, Eq, Ord, Generic)

instance A.ToJSON Direction

data Patrol = Patrol { patrolY    :: !Int
                     , patrolX    :: !Int
                     , patrolDir  :: !Direction
                     } deriving (Show, Eq, Ord, Generic)

instance A.ToJSON Patrol

data Objects = Objects { obst    :: S.Set (Int, Int) -- 🍐🍎
                       , patrols :: ![Patrol]
                       } deriving (Show, Generic)

instance A.ToJSON Objects

-- Parsing

cell :: Grid Objects -> Parser Objects
cell g = (anyChar >>= patrol >>= pure . addPatrol g) <|>
         (anyChar >>= obstacle >>= addObstacle g)

addObstacle :: Applicative f => Grid Objects -> Bool -> f Objects
addObstacle Grid{..} True = pure stuff{obst = S.insert (rows, trail) $ obst stuff}
addObstacle Grid{..} False = pure stuff

addPatrol :: Grid Objects -> Direction -> Objects
addPatrol Grid{..} d = stuff{patrols = Patrol rows trail d : patrols stuff}

patrol :: Alternative f => Char -> f Direction
patrol c = case c of
  '<' -> pure ToLeft
  '>' -> pure ToRight
  '^' -> pure ToUp
  'v' -> pure ToDown
  _   -> empty

obstacle :: Alternative f => Char -> f Bool
obstacle c = case c of
  '#' -> pure True
  '.' -> pure False
  _   -> empty

-- Now the implementation

patrolFwd :: Patrol -> Patrol
patrolFwd Patrol{..} = f $ case patrolDir of
  ToLeft  -> (  0, -1)
  ToRight -> (  0,  1)
  ToUp    -> ( -1,  0)
  ToDown  -> (  1,  0)
  where f (dy, dx) = Patrol { patrolY = patrolY + dy
                            , patrolX = patrolX + dx
                            , ..
                            }

turnRight :: Direction -> Direction
turnRight d = case d of
  ToLeft  -> ToUp
  ToUp    -> ToRight
  ToRight -> ToDown
  ToDown  -> ToLeft

movePatrol :: Grid Objects -> Patrol -> Patrol
movePatrol Grid{..} origP = if S.member (patrolY movedP, patrolX movedP) $ obst stuff
                            then origP{ patrolDir = turnRight (patrolDir origP) }
                            else movedP
  where movedP = patrolFwd origP

isInside :: Grid a -> Patrol -> Bool
isInside Grid{..} Patrol{..} = patrolY >= 0 && patrolY < rows &&
                               patrolX >= 0 && patrolX < cols

travel :: Grid Objects -> Patrol -> [Patrol]
travel grid pat = if isIn
                  then next : travel grid next
                  else []
  where next = movePatrol grid pat
        isIn = isInside grid next

toLocation :: Patrol -> (Int, Int)
toLocation Patrol{..} = (patrolY, patrolX)

-- |A loop is something where the guard travels to the same direction
-- at the same location.
hasLoop :: Ord a => S.Set a -> [a] -> Bool
hasLoop s (x:xs) = if S.member x s
                    then True
                    else hasLoop (S.insert x s) xs
hasLoop _ []      = False -- Guard walked out

gridHasLoop :: Grid Objects -> Bool
gridHasLoop grid = hasLoop mempty $ travel grid $ singlePatrol grid

singlePatrol :: Grid Objects -> Patrol
singlePatrol g = case patrols (stuff g) of
  [a] -> a
  _   -> error "Expecting exactly 1 patrol"

addObsts :: Grid Objects -> [Grid Objects]
addObsts Grid{..} = [ Grid{stuff = stuff{obst = S.insert (y, x) (obst stuff)}, ..}
                    | y <- [0..rows-1]
                    , x <- [0..cols-1]
                    , not $ any (isStart y x) $ patrols stuff
                    , S.notMember (y,x) $ obst stuff
                    ]
  where isStart y x Patrol{..} = patrolY == y && patrolX == x
