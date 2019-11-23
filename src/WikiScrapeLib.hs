{-# LANGUAGE OverloadedStrings #-}

module WikiScrapeLib
    (  mostfrequentwordonpage, articleBody, test, removePunct, countOccurrences, getCounts, getCountTuples, getStopWords, filterStopWords
    ) where

import Text.HTML.Scalpel
import Data.List.Split
import Control.Monad.Trans.Maybe
import Data.List (nub)

test :: IO (Maybe [String])
test = do
    words <- runMaybeT $ do
        article <- articleBody "https://en.wikipedia.org/wiki/Germany"
        stopWords <- getStopWords
        let wordList = removePunct <$> (splitWords article)
        let filteredWords = filterStopWords wordList stopWords
        return filteredWords
    return words


-- mostfrequentwordonpage :: URL -> IO (Maybe String)
-- mostfrequentwordonpage page = do
--   return (Just "fixme")

mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage page = do
    word <- runMaybeT $ do
        article <- articleBody page
        stopWords <- getStopWords
        let wordList = removePunct <$> (splitWords article)
        let filteredWords = filterStopWords wordList stopWords
        let maxWord = snd $ maximum $ getCountTuples filteredWords
        return maxWord
    return word

articleBody :: URL -> MaybeT IO String
articleBody url = MaybeT $ do
    let
        article :: Scraper String String
        article = text ("div" @: ["id" @= "mw-content-text"])
    scrapeURL url article

splitWords :: String -> [String]
splitWords t = splitOn " " t

removePunct :: String -> String
removePunct s = filter (/= '\n') s

countOccurrences :: String -> [String] -> Int
countOccurrences x = length . filter (x==)

-- Full list to search for, items to search
getCounts :: [String] -> [String] -> [Int]
getCounts l unique = foldr (\x buff -> (countOccurrences x l) : buff) [] unique

getCountTuples :: [String] -> [(Int, String)]
getCountTuples l = zip (getCounts l (nub l)) (nub l)

getStopWords :: MaybeT IO ([String])
getStopWords = MaybeT $ do  
    contents <- readFile "stopwords.txt"
    stopWords <- return ("a" : (splitOn "\n" contents))
    if length stopWords < 2
        then return Nothing
        else return $ Just stopWords

-- List, stopwords
filterStopWords :: [String] -> [String] -> [String]
filterStopWords l stopwords = filter (\x -> not (x `elem` stopwords)) l
