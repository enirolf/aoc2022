module AOC.Day4 where

import AOC.Util

import Data.List (union, intersect, sort)
import Data.List.Split (splitOn)

type Section = Int

string2sections :: String -> [Section]
string2sections str = case splitOn "-" str of
  [b, e] -> [(read b)..(read e)]
  _      -> error "invalid section"

splitPairs :: String -> [String]
splitPairs = splitOn ","

sectionsPerPair :: String -> [[Section]]
sectionsPerPair str = map string2sections (splitPairs str)

fullyContained :: [[Section]] -> Bool
fullyContained [sec1, sec2] = fullyContained' sec1 sec2
fullyContained (sec1:sec2:secs) = fullyContained' sec1 sec2 && fullyContained (union sec1 sec2:secs)
fullyContained _ = False

fullyContained' :: [Section] -> [Section] -> Bool
fullyContained' s1 s2 = sortedUnion == s1 || sortedUnion == s2
  where sortedUnion = sort $ s1 `union` s2

overlaps :: [[Section]] -> Bool
overlaps [sec1, sec2] = overlaps' sec1 sec2
overlaps (sec1:sec2:secs) = overlaps' sec1 sec2 && overlaps (sec1 `union` sec2 : secs)
overlaps _ = False

overlaps' :: [Section] -> [Section] -> Bool
overlaps' s1 s2 = (s1 `intersect` s2) /= []

day4a :: Mode -> IO ()
day4a mode = do
  input <- case mode of
    Test -> readFileStrings "data/day4_test.txt"
    Full -> readFileStrings "data/day4.txt"
  let sections = map sectionsPerPair input
  print $ length (filter fullyContained sections)

day4b :: Mode -> IO ()
day4b mode = do
  input <- case mode of
    Test -> readFileStrings "data/day4_test.txt"
    Full -> readFileStrings "data/day4.txt"
  let sections = map sectionsPerPair input
  print $ length (filter overlaps sections)
