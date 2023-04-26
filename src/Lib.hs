module Lib
    ( getMostProbable
    , t
    , getMap
    , preprocess
    ) where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L
import Data.Ord (comparing, Down(..))

t = T.pack "Here is some sample text, this sample text has punctuation!!"


getMostProbable :: T.Text -> T.Text -> [(T.Text, Int)]
getMostProbable key text = case M.lookup key mapped of
                         Just grams -> L.sortBy (comparing (Down . snd)) (M.toList occurencesMap)
                           where occurencesMap = M.fromListWith (+) [(word, 1) | word <- grams]
                         Nothing -> [(T.empty,0)]
  where mapped = getMap 2 (preprocess text)

preprocess :: T.Text -> [T.Text]
preprocess text = T.words . T.toLower . removePunctuation $ text

removePunctuation :: T.Text -> T.Text
removePunctuation = T.filter (not . (`T.elem` punctuation))
  where punctuation = T.pack ",.?!-:;\"\'"

getMap :: Int -> [T.Text] -> M.Map T.Text [T.Text]
getMap n xs = M.fromListWith (++) transformed
  where
    transformed = [(T.intercalate (T.pack " ") (init gram), [last gram]) | gram <- ngrams]
    ngrams = ngram (n + 1) xs

ngram :: Int -> [T.Text] -> [[T.Text]]
ngram n xs 
  | n <= length xs = take n xs : ngram n (drop 1 xs)
  | otherwise = []
