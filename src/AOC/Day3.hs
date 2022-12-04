module AOC.Day3 where

import AOC.Util

import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Char (isLower, isUpper, ord)

type Items = String
type Item = Char

type Elves = [Items]
type Group = [Items]

splitCompartments :: Items -> (Items, Items)
splitCompartments rucksack = splitAt (length rucksack `div` 2) rucksack

sharedItem :: [Items] -> Item
sharedItem [i1, i2] = head $ intersect i1 i2
sharedItem (i1:i2:is) = sharedItem $ intersect i1 i2 : is
sharedItem _ = error "no shared items"

priority :: Item -> Int
priority item | isLower item = ord item - 96
              | isUpper item = ord item - 38
              | otherwise    = error "invalid item"

divideGroups :: Elves -> [Group]
divideGroups = chunksOf 3

-- sharedBadge :: [Group] ->

day3a :: Mode -> IO ()
day3a mode = do
  input <- case mode of
    Test -> readFileStrings "data/day3_test.txt"
    Full -> readFileStrings "data/day3.txt"
  let compartmentItems = map splitCompartments input
  let sharedItems = map (\ (c1, c2) -> sharedItem [c1, c2]) compartmentItems
  print $ sum $ map priority sharedItems

day3b :: Mode -> IO ()
day3b mode = do
  input <- case mode of
    Test -> readFileStrings "data/day3_test.txt"
    Full -> readFileStrings "data/day3.txt"
  let groups = divideGroups input
  let badges = map sharedItem groups
  print $ sum $ map priority badges
