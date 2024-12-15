{-# LANGUAGE DeriveGeneric, RecordWildCards, TupleSections #-}
module Day15 where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (takeWhile)
import Data.List (unfoldr)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Aeson (ToJSON)
import GHC.Generics
import Grid
import GridTools

import Day

task :: Day Input Int
task = Day { parser  = everything
           , solvers = [ part1 $ score . last . iterateJust generation
                       ]
           }

data Input = Input { arena      :: M.Map Coord Obj
                   , robotPos   :: Coord
                   , directions :: [Coord]
                   } deriving (Show, Generic)

data Obj = Wall | Box deriving (Show, Generic)

data Arena = Arena { robots  :: [Coord]
                   , arena'  :: M.Map Coord Obj
                   } deriving (Show, Generic)

instance ToJSON Obj
instance ToJSON Input

-- Parsing

everything :: Parser Input
everything = do
  Grid{..} <- gridParser cell (Arena mempty mempty)
  directions <- many $ ignoreBreak direction
  endOfLine
  endOfInput
  robotPos <- case robots stuff of
    [a] -> pure a
    _   -> fail "Incorrect amount of robots, expecting only one"
  pure Input{ arena = arena' stuff, ..}

ignoreBreak :: Parser p -> Parser p
ignoreBreak p = p <|> (endOfLine >> p)

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

direction :: Parser Coord
direction = anyChar >>= \c -> case c of
  '<' -> pure (-1,  0)
  '>' -> pure ( 1,  0)
  '^' -> pure ( 0, -1)
  'v' -> pure ( 0,  1)
  _   -> empty

-- Sokoban part begins

iterateJust :: (a -> Maybe a) -> a -> [a]
iterateJust f = unfoldr (fmap wrap . f)
  where wrap x = (x, x)

generation :: Input -> Maybe Input
generation old@Input{..} = case directions of
  (dir:rest) -> let tryPos = addCoord robotPos dir
                in case push tryPos dir arena of
    Just newArena -> Just Input{ directions = rest
                               , robotPos = tryPos
                               , arena = newArena
                               }
    Nothing -> Just old{directions = rest} -- Nothing moved
  [] -> Nothing -- End of input

-- |Push boxes at given direction or fail to do so
push :: Coord -> Coord -> M.Map Coord Obj -> Maybe (M.Map Coord Obj)
push pos d m = case m M.!? pos of
  Nothing   -> Just m  -- Free space, return self
  Just Wall -> Nothing -- None shall pass
  Just Box  -> move <$> push next d m
  where move = M.insert next Box . M.delete pos
        next = addCoord pos d

gps :: (Coord, Obj) -> Int
gps (_, Wall)     = 0
gps ((x, y), Box) = x + 100*y

score :: Input -> Int
score = sum . map gps . M.toList . arena
