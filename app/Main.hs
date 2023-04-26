{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib


import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Text as T

data Person = Person
  { id     :: !Int
  , gender :: !String
  , age    :: !Int
  , topic  :: !String
  , sign   :: !String
  , date   :: !String
  , text   :: !String
}

instance FromNamedRecord Person where
  parseNamedRecord r = Person <$> r .: "id"
                              <*> r .: "gender"
                              <*> r .: "age"
                              <*> r .: "topic"
                              <*> r .: "sign"
                              <*> r .: "date"
                              <*> r .: "text"

main :: IO ()
main = do
  csvData <- BL.readFile "dataset/blogtext.csv"
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, v) -> V.forM_ v $ \ p ->
      putStrLn $ text p
