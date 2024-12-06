{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Day06 where

import Text.Printf
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Set as S
import qualified Data.Aeson as A
import Control.Parallel.Strategies
import GHC.Generics

import Day

task :: Day Grid Int
task = Day { parser = cells emptyGrid
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

data Grid = Grid { obst    :: S.Set (Int, Int) -- 🍐🍎
                 , patrols :: ![Patrol]
                 , rows    :: !Int
                 , cols    :: !Int
                 , trail   :: !Int
                 } deriving (Show, Generic)

instance A.ToJSON Grid

-- Parsing

emptyGrid :: Grid
emptyGrid = Grid mempty mempty 0 0 0

cells :: Grid -> Parser Grid
cells g = (endOfInput >> pure g) <|> (cell g >>= cells)

cell :: Grid -> Parser Grid
cell g = choice [ endOfLine >> nextRow g
                , anyChar >>= patrol >>= addPatrol g >>= nextCol
                , anyChar >>= obstacle >>= addObstacle g >>= nextCol
                ]

addObstacle :: Applicative f => Grid -> Bool -> f Grid
addObstacle Grid{..} True = pure Grid{obst = S.insert (rows, trail) obst, ..}
addObstacle g False = pure g

addPatrol :: Applicative f => Grid -> Direction -> f Grid
addPatrol Grid{..} d = pure Grid{patrols = Patrol rows trail d : patrols, ..}

nextCol :: Applicative f => Grid -> f Grid
nextCol Grid{..} = pure Grid{trail = trail + 1, ..}

nextRow :: MonadFail f => Grid -> f Grid
nextRow Grid{..} = if rows == 0 || cols == trail
                   then pure Grid{ rows = rows + 1
                                 , cols = trail
                                 , trail = 0
                                 , ..
                                 }
                   else fail $ printf "Row %d length %d is uneven" rows trail

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

movePatrol :: Grid -> Patrol -> Patrol
movePatrol Grid{..} origP = if S.member (patrolY movedP, patrolX movedP) obst
                            then origP{ patrolDir = turnRight (patrolDir origP) }
                            else movedP
  where movedP = patrolFwd origP

isInside :: Grid -> Patrol -> Bool
isInside Grid{..} Patrol{..} = patrolY >= 0 && patrolY < rows &&
                               patrolX >= 0 && patrolX < cols

travel :: Grid -> Patrol -> [Patrol]
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

gridHasLoop :: Grid -> Bool
gridHasLoop grid = hasLoop mempty $ travel grid $ singlePatrol grid

singlePatrol :: Grid -> Patrol
singlePatrol Grid{..} = case patrols of
  [a] -> a
  _   -> error "Expecting exactly 1 patrol"

addObsts :: Grid -> [Grid]
addObsts Grid{..} = [ Grid{obst = S.insert (y, x) obst, ..}
                    | y <- [0..rows-1]
                    , x <- [0..cols-1]
                    , not $ any (isStart y x) patrols
                    , S.notMember (y,x) obst
                    ]
  where isStart y x Patrol{..} = patrolY == y && patrolX == x
