module AOC.Day1 where

import AOC.Util

import Data.List (sortBy)

totalCalories :: [[Int]] -> [Int]
totalCalories = map sum

maxCalories :: [Int] -> Int
maxCalories = maximum

topThreeCalories :: [Int] -> [Int]
topThreeCalories cals = take 3 $ sortBy (flip compare) cals

day1a :: Mode -> IO ()
day1a mode = do
  input <- case mode of
    Test -> chunkStrs <$> readFileStrings "data/day1_test.txt"
    Full -> chunkStrs <$> readFileStrings "data/day1.txt"
  let caloriesPerElf = map strs2ints input
  print $ (maxCalories . totalCalories) caloriesPerElf

day1b :: Mode -> IO ()
day1b mode = do
  input <- case mode of
    Test -> chunkStrs <$> readFileStrings "data/day1_test.txt"
    Full -> chunkStrs <$> readFileStrings "data/day1.txt"
  let caloriesPerElf = map strs2ints input
  print $ (sum . topThreeCalories . totalCalories) caloriesPerElf

