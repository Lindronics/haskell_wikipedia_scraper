{-# LANGUAGE OverloadedStrings #-}

module WikiScrapeLib
    (  mostfrequentwordonpage, articleBody, test, removePunct, getCountTuples, getStopWords, filterStopWords, isNumerical
    ) where

import Text.HTML.Scalpel
import Data.List.Split (splitOn)
import Control.Monad.Trans.Maybe
import Data.List (nub, intercalate)
import Data.Char (toLower)
import Control.Exception
import Control.Applicative

test :: IO (Maybe [String])
test = do
    words <- runMaybeT $ do
        (article, css) <- articleBody "https://en.wikipedia.org/wiki/Ruritania"
        -- css <- cssJunk "https://en.wikipedia.org/wiki/Ruritania"
        stopWords <- getStopWords

        let wordList = splitOn " " $ toLower <$> (removePunct article)
        let cssList = splitOn " " $ toLower <$> removePunct css
        return cssList
        
        -- let filterList = stopWords ++ (return <$> ['a'..'z'])
        -- let name = toLower <$> "ruritania"
        -- let filteredWords = (filterNumerical.(filterName name).(flip filterStopWords filterList)) wordList

        -- -- return filteredWords
        -- return $ getCountTuples filteredWords
    return words


-- mostfrequentwordonpage :: URL -> IO (Maybe String)
-- mostfrequentwordonpage page = do
--   return (Just "fixme")

{-|
    Returns the most frequent word on a given Wikipedia page.
-}
mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage page = do
    runMaybeT $ do
        -- Load Wikipedia article and stop words
        (article, css) <- articleBody page
        stopWords <- getStopWords

        -- Remove punct first, then convert to lower and split on whitespace
        let wordList = splitOn " " $ toLower <$> removePunct article
        let cssList = splitOn " " $ toLower <$> removePunct css

        -- Remove stopwords, single characters, and words containing name
        let filterList = stopWords ++ cssList ++ (return <$> ['a'..'z'])
        let name = toLower <$> (last $ splitOn "/" page)
        let filteredWords = (filterNumerical.(filterName name).(flip filterStopWords filterList)) wordList

        -- Return most used word
        return $ snd $ maximum $ getCountTuples filteredWords


articleBody :: URL -> MaybeT IO (String, String)
articleBody url = MaybeT $ do
    let
        article :: Scraper String (String, String)
        article = do 
            content <- text ("div" @: ["id" @= "mw-content-text"])
            css <- texts "style"
            return (content, concat css)

        junk :: Scraper String [String]
        junk = texts "style"

        catchAny :: IO a -> (SomeException -> IO a) -> IO a
        catchAny = Control.Exception.catch

    catchAny (scrapeURL url article) (\e -> return Nothing)


removePunct :: String -> String
removePunct s = let
    replace :: Char -> Char
    replace x
        | x `elem` (['a'..'z'] ++ ['A'..'Z']  ++ ['0'..'9'] :: String) = x
        | otherwise = ' '
    in
        replace <$> s

countOccurrences :: String -> [String] -> Int
countOccurrences x = length . filter (x==)

-- |Full list to search for, items to search
getCounts :: [String] -> [String] -> [Int]
getCounts l unique = foldr (\x buff -> (countOccurrences x l) : buff) [] unique

getCountTuples :: [String] -> [(Int, String)]
getCountTuples l = zip (getCounts l (nub l)) (nub l)

getStopWords :: MaybeT IO ([String])
getStopWords = MaybeT $ do  
    contents <- readFile "stopwords.txt"
    stopWords <- return (splitOn "\n" contents)
    if length stopWords < 1
        then return Nothing
        else return $ Just stopWords

-- List, stopwords
filterStopWords :: [String] -> [String] -> [String]
filterStopWords l stopWords = filter (\x -> (not (x `elem` stopWords)) && length l > 0) l

filterName :: String -> [String] -> [String]
filterName name l = filter (\x -> not (take 4 x == take 4 name)) l

isNumerical :: String -> Bool
isNumerical w = foldr (\x acc -> (x `elem` ['0'..'9']) && acc) True w

filterNumerical :: [String] -> [String]
filterNumerical l = filter (not.isNumerical) l
