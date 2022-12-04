module AOC.Util where

import Data.List.Split (splitOn)

data Mode = Test | Full deriving (Read, Show)

readFileStrings :: FilePath -> IO [String]
readFileStrings fp = do
  contents <- readFile fp
  return $ lines contents

readFileInts :: FilePath -> IO [Int]
readFileInts fp = do
  contents <- readFile fp
  return $ map read $ lines contents

chunkStrs :: [String] -> [[String]]
chunkStrs = splitOn [""]

strs2ints :: [String] -> [Int]
strs2ints = map read

