module Day2 where

import Util

------------------------------------ Part 1 ------------------------------------

data Shape = Rock | Paper | Scissors deriving (Eq, Read, Show)

instance Ord Shape where
  compare Rock Paper = LT
  compare Paper Rock = GT
  compare Rock Scissors = GT
  compare Scissors Rock = LT
  compare Scissors Paper = GT
  compare Paper Scissors = LT
  compare _ _ = EQ

data Round    = Round Shape Shape
                deriving (Read, Show)

string2round :: String -> Round
string2round str = case words str of
                    [o, p] -> Round (shape o) (shape p)
                    _      -> error "invalid format"
  where shape s | s == "A" || s == "X" = Rock
                | s == "B" || s == "Y" = Paper
                | s == "C" || s == "Z" = Scissors
                | otherwise = error "unknown shape"

shapeScore :: Shape -> Int
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

outcomeScore :: Round -> Int
outcomeScore (Round o p) | o > p  = 0
                         | o == p = 3
                         | o < p  = 6
                         | otherwise = error "unknown outcome"

roundScore :: Round -> Int
roundScore r@(Round _ p) = shapeScore p + outcomeScore r

totalScore :: [Round] -> Int
totalScore rs = sum $ map roundScore rs

------------------------------------ Part 2 ------------------------------------

data Outcome = Win | Draw | Lose deriving (Read, Show)

data RoundDirective = RoundDirective Shape Outcome
                      deriving (Read, Show)

requiredShape :: RoundDirective -> Shape
requiredShape (RoundDirective s Win)  = case s of Rock     -> Paper
                                                  Paper    -> Scissors
                                                  Scissors -> Rock
requiredShape (RoundDirective s Lose) = case s of Rock     -> Scissors
                                                  Paper    -> Rock
                                                  Scissors -> Paper
requiredShape (RoundDirective s Draw) = s

string2roundDir :: String -> RoundDirective
string2roundDir str = case words str of
                        [s, o] -> RoundDirective (shape s) (outcome o)
                        _      -> error "invalid format"
  where shape s'   | s' == "A" = Rock
                   | s' == "B" = Paper
                   | s' == "C" = Scissors
                   | otherwise = error "unknown shape"
        outcome o' | o' == "X" = Lose
                   | o' == "Y" = Draw
                   | o' == "Z" = Win
                   | otherwise = error "unknown outcome"

roundDir2Round :: RoundDirective -> Round
roundDir2Round rd@(RoundDirective s _) = Round s (requiredShape rd)

day2a :: IO ()
day2a = do
  input <- readFileStrings "data/day2.txt"
  let rounds = map string2round input
  print $ totalScore rounds

day2b :: IO ()
day2b = do
  input <- readFileStrings "data/day2.txt"
  let rounds = map (roundDir2Round . string2roundDir) input
  print $ totalScore rounds

