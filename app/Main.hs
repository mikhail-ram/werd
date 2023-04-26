{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib


import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Text as T

{-
data Person = Person
  { id     :: !Int
  , gender :: !String
  , age    :: !Int
  , topic  :: !String
  , sign   :: !String
  , date   :: !String
  , text   :: !String
}
-}

data Blog = Blog {
  text :: T.Text
} deriving (Show, Generic)

instance FromNamedRecord Blog

decodeBlog :: BL.ByteString -> Either String (Header, V.Vector Blog)
decodeBlog = decodeByName

{-
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
  let text = T.empty
  csvData <- BL.readFile "dataset/blogtext.csv"
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, v) -> V.forM_ v $ \ p ->
      putStrLn $ text p
-}

main :: IO ()
main = do
  csvData <- BL.readFile "dataset/blogtext.csv"
  case decodeBlog csvData of
    Left err -> putStrLn err
    Right (_, v) -> print $ getMostProbable "hello" (T.intercalate (T.pack " ") (V.toList (V.map text v)))
