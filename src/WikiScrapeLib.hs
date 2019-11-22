{-# LANGUAGE OverloadedStrings #-}

module WikiScrapeLib
    (  mostfrequentwordonpage, articleBody, test, removePunct, countOccurrences, getCounts, getCountTuples, getMaxWord, getStopWords, filterStopWords
    ) where

import Text.HTML.Scalpel
import Data.List.Split
import Control.Monad.Trans.Maybe
import Data.List (nub)

test :: IO (Maybe [String])
test = do
    wordList <- (fmap.fmap) splitWords (articleBody "https://en.wikipedia.org/wiki/Germany")
    strippedWords <- case wordList of
        Nothing -> return Nothing
        Just wordList -> return (Just (fmap removePunct wordList))
    stopWords <- getStopWords

    filteredWords <- case strippedWords of
        Nothing -> return Nothing
        Just strippedWords -> return (Just (filterStopWords strippedWords stopWords))
    
    return filteredWords


-- mostfrequentwordonpage :: URL -> IO (Maybe String)
-- mostfrequentwordonpage page = do
--   return (Just "fixme")

-- TODO use Monad transformers to avoid Maybe checking
mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage page = do
    wordList <- (fmap.fmap) splitWords (articleBody page)
    strippedWords <- case wordList of
        Nothing -> return Nothing
        Just wordList -> return (Just (fmap removePunct wordList))

    stopWords <- getStopWords

    filteredWords <- case strippedWords of
        Nothing -> return Nothing
        Just strippedWords -> return (Just (filterStopWords strippedWords stopWords))

    maxWord <- case filteredWords of 
        Nothing -> return Nothing
        Just filteredWords -> return (Just (getMaxWord filteredWords))
    return maxWord

articleBody :: URL -> IO (Maybe String)
articleBody url = scrapeURL url article
    where
        article :: Scraper String String
        article = text ("div" @: ["id" @= "mw-content-text"])


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

getMaxWord :: [String] -> String
getMaxWord l = snd $ maximum (getCountTuples l)

getStopWords :: IO ([String])
getStopWords = do  
    contents <- readFile "stopwords.txt"
    stopWords <- return ("a" : (splitOn "\n" contents))
    return stopWords

-- List, stopwords
filterStopWords :: [String] -> [String] -> [String]
filterStopWords l stopwords = filter (\x -> not (x `elem` stopwords)) l
