module Util where

import Data.List.Split

readFileStrings :: FilePath -> IO [[String]]
readFileStrings fp = do
  contents <- readFile fp
  return $ chunkStrs $ lines contents

readFileInts :: FilePath -> IO [Int]
readFileInts fp = do
  contents <- readFile fp
  return $ map read $ lines contents

chunkStrs :: [String] -> [[String]]
chunkStrs strs = splitOn [""] strs

strs2ints :: [String] -> [Int]
strs2ints = map read

