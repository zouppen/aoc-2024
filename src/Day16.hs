{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Day16 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.List (unfoldr)
import qualified Data.Map as M

import Day
import Grid
import Wastl (direction, ignoreNewline)

task :: Day Game Int
task = Day { parser  = everything
           , solvers = [ part 1 $ minimum . map score . filter isWon . race
                       ]
           }

data Arena = Arena { deers  :: [Coord]
                   , maze'  :: M.Map Coord Obj
                   } deriving (Show)

data Game = Game { maze    :: M.Map Coord Obj
                 , deerPos :: Coord
                 , deerDir :: Coord
                 , score   :: Int
                 } deriving (Show, Generic)

data Obj = Wall | End | BeenHere deriving (Show, Eq,Generic)

instance ToJSON Game
instance ToJSON Obj

-- Parsing

everything :: Parser Game
everything = do
  Grid{..} <- gridParser cell (Arena mempty mempty)
  directions <- many $ ignoreNewline direction
  endOfInput
  deerPos <- case deers stuff of
    [a] -> pure a
    _   -> fail "Incorrect amount of reindeers, expecting only one"
  pure Game{ maze = maze' stuff
           , deerPos = deerPos
           , deerDir = (1,0) -- East, hardcoded in puzzle
           , score = 0
           }

cell :: Grid Arena -> Parser Arena
cell g = anyChar >>= \c -> case c of
  '#' -> pure $ item Wall
  'E' -> pure $ item End
  'S' -> pure old{deers = pos:deers old}
  '.' -> pure old -- Empty cells are not populated
  _   -> empty
  where old = stuff g
        pos = gridCoord g
        item a = old{ maze' = M.insert pos a $ maze' old }

-- Race begins

race :: Game -> [Game]
race g@Game{..} = if M.member deerPos maze
                  then [g] -- Crashed into something
                  else [ game
                       | move <- [(id, 1), (turnLeft, 1001), (turnRight, 1001)]
                       , game <- race $ tryMove move g
                       ]

tryMove :: ((Coord -> Coord), Int) -> Game -> Game
tryMove (turner, penalty) Game{..} =
  let newDir = turner deerDir
  in Game{ maze = M.insert deerPos BeenHere maze
         , deerPos = addCoord deerPos newDir
         , deerDir = newDir
         , score = score + penalty
         }

isWon :: Game -> Bool
isWon Game{..} = M.lookup deerPos maze == Just End


renderGame :: Game -> String
renderGame Game{..} =
  [ if x == maxX+1
    then '\n'
    else if deerPos == (x, y)
         then '@'
         else case (M.lookup (x, y) maze) of
                Just Wall     -> '#'
                Just End      -> 'E'
                Just BeenHere -> 'o'
                Nothing -> '.'
  | y <- [0..maxY]
  , x <- [0..maxX+1]
  ]
  where ((maxX, maxY),_) = M.findMax maze
