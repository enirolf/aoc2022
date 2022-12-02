module Day1 where

import Util

import Data.List

totalCalories :: [[Int]] -> [Int]
totalCalories = map sum

maxCalories :: [Int] -> Int
maxCalories = maximum

topThreeCalories :: [Int] -> [Int]
topThreeCalories cals = take 3 $ sortBy (flip compare) cals

day1a :: IO ()
day1a = do
  input <- chunkStrs <$> readFileStrings "data/day1.txt"
  let caloriesPerElf = map strs2ints input
  print $ (maxCalories . totalCalories) caloriesPerElf

day1b :: IO ()
day1b = do
  input <- chunkStrs <$> readFileStrings "data/day1.txt"
  let caloriesPerElf = map strs2ints input
  print $ (sum . topThreeCalories . totalCalories) caloriesPerElf

