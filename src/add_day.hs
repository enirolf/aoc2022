{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import System.Environment (getArgs)
import System.Process (callCommand)
import Data.Aeson (ToJSON, toJSON)
import Data.Text (Text, pack)
import GHC.Generics
import Text.Mustache
import qualified Data.Text.Lazy.IO as TIO

newtype Day = Day { day :: Text }
              deriving (Eq, Show, Generic)

instance ToJSON Day


main :: IO ()
main = do
  dayArg <- getArgs
  case dayArg of
    []    -> putStrLn "Please provide the day to generate!"
    (d:_) -> do
      let dayJSON = toJSON $ Day (pack d)
      let hsFile = "src/AOC/Day" ++ d ++ ".hs"
      let dataFile = "data/day" ++ d ++ ".txt"
      let dataTestFile = "data/day" ++ d ++ "_test.txt"
      templ <- compileMustacheFile "day_template.mustache"
      TIO.writeFile hsFile $ renderMustache templ dayJSON
      TIO.writeFile dataFile "add data here!"
      TIO.writeFile dataTestFile "add data here!"
      callCommand "stack build > /dev/null 2>&1"
      putStrLn $ "Created " ++ hsFile ++ " and data files (make sure to add actual data!!)"




