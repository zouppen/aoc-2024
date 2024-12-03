{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Day03 where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Aeson (ToJSON)
import GHC.Generics

import Day

task :: Day [Stmt] Int
task = Day { parser = stmts
           , solvers = [("part1", ShowSolver $ total . part2 . filter (/= StmtDont))
                       ,("part2", ShowSolver $ total . part2)
                       ]
           }

-- Types

data Stmt = StmtDo
          | StmtDont
          | Mul Int Int
          deriving (Show, Eq, Generic)

instance ToJSON Stmt

data State = State { isEnabled :: Bool
                   , total     :: Int
                   } deriving (Show)

-- Parsing tools

skipUntil :: Parser a -> Parser a
skipUntil p = p <|> (anyChar >> (skipUntil p))

mul :: Parser Stmt
mul = do
  _ <- string "mul("
  a <- decimal
  _ <- char ','
  b <- decimal
  _ <- string ")"
  pure $ Mul a b

stmtDo :: Parser Stmt
stmtDo = string "do()" >> pure StmtDo

stmtDont :: Parser Stmt
stmtDont = string "don't()" >> pure StmtDont

stmt :: Parser Stmt
stmt = mul <|> stmtDo <|> stmtDont

stmts :: Parser [Stmt]
stmts = many $ skipUntil stmt

-- Magic

evalStmt :: State -> Stmt -> State
evalStmt State{..} s = case s of
  StmtDo   -> State{isEnabled = True, ..}
  StmtDont -> State{isEnabled = False, ..}
  Mul a b  -> if isEnabled
              then State{total = total + (a*b), ..}
              else State{..}

part2 :: Foldable t => t Stmt -> State
part2 = foldl evalStmt State{isEnabled = True, total = 0}
