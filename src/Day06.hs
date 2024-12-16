{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
module Day06 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Set as S
import Control.Parallel.Strategies

import Day
import Grid

task :: Day (Grid Objects) Int
task = Day { parser  = gridParser cell $ Objects mempty mempty
           , solvers = [ part 1 $ length . S.fromList . map toLocation . \a -> travel a $ singlePatrol a
                       , part 2 $ length . filter id .
                         withStrategy (parListChunk 12 rseq) .
                         map gridHasLoop . addObsts
                       ]
           }

data Patrol = Patrol { patrolX    :: !Int
                     , patrolY    :: !Int
                     , patrolDir  :: !Coord
                     } deriving (Show, Eq, Ord, Generic)

instance ToJSON Patrol

data Objects = Objects { obst    :: S.Set (Int, Int) -- 🍐🍎
                       , patrols :: ![Patrol]
                       } deriving (Show, Generic)

instance ToJSON Objects

-- Parsing

cell :: Grid Objects -> Parser Objects
cell g = (anyChar >>= patrol >>= pure . addPatrol g) <|>
         (anyChar >>= obstacle >>= addObstacle g)

addObstacle :: Applicative f => Grid Objects -> Bool -> f Objects
addObstacle Grid{..} True = pure stuff{obst = S.insert (trail, rows) $ obst stuff}
addObstacle Grid{..} False = pure stuff

addPatrol :: Grid Objects -> Coord -> Objects
addPatrol Grid{..} d = stuff{patrols = Patrol trail rows d : patrols stuff}

patrol :: Alternative f => Char -> f Coord
patrol c = case c of
  '<' -> pure ( -1,  0)
  '>' -> pure (  1,  0)
  '^' -> pure (  0, -1)
  'v' -> pure (  0,  1)
  _   -> empty

obstacle :: Alternative f => Char -> f Bool
obstacle c = case c of
  '#' -> pure True
  '.' -> pure False
  _   -> empty

-- Now the implementation

patrolFwd :: Patrol -> Patrol
patrolFwd Patrol{..} = f patrolDir
  where f (dx, dy) = Patrol { patrolX = patrolX + dx
                            , patrolY = patrolY + dy
                            , ..
                            }

turnRight :: Coord -> Coord
turnRight (x, y) = (-y, x)

movePatrol :: Grid Objects -> Patrol -> Patrol
movePatrol Grid{..} origP = if S.member (patrolX movedP, patrolY movedP) $ obst stuff
                            then origP{ patrolDir = turnRight (patrolDir origP) }
                            else movedP
  where movedP = patrolFwd origP

travel :: Grid Objects -> Patrol -> [Patrol]
travel grid pat = if isIn
                  then next : travel grid next
                  else []
  where next = movePatrol grid pat
        isIn = bounds grid (patrolX next, patrolY next)

toLocation :: Patrol -> (Int, Int)
toLocation Patrol{..} = (patrolX, patrolY)

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
addObsts Grid{..} = [ Grid{stuff = stuff{obst = S.insert (x, y) (obst stuff)}, ..}
                    | y <- [0..rows-1]
                    , x <- [0..cols-1]
                    , not $ any (isStart x y) $ patrols stuff
                    , S.notMember (x, y) $ obst stuff
                    ]
  where isStart x y Patrol{..} = patrolX == x && patrolY == y
