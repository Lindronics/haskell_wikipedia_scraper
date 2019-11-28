{-# LANGUAGE OverloadedStrings #-}


module WikiScrapeLib
    (  mostfrequentwordonpage, articleBody, occurrences, removePunct, getStopWords, filterStopWords, isNumerical
    ) where


import Text.HTML.Scalpel
import Data.List.Split (splitOn)
import Control.Monad.Trans.Maybe
import Data.List (nub)
import Data.Char (toLower)
import Control.Exception
import qualified Data.Map as Map
import Data.Tuple (swap)


{-|
    Returns the most frequent word on a given Wikipedia page.
-}
mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage page = do
    runMaybeT $ do
        -- Load Wikipedia article, css junk and stop words
        (article, css) <- articleBody page
        stopWords <- getStopWords

        -- Remove punct first, then convert to lower and split on whitespace
        let wordList = splitOn " " $ toLower <$> removePunct article
        let cssList = splitOn " " $ toLower <$> removePunct css

        -- Remove stopwords, css junk, single characters, and words containing name
        let filterList = stopWords ++ cssList ++ (return <$> ['a'..'z'])
        let name = toLower <$> (last $ splitOn "/" page)
        let filteredWords = (filterNumerical.(filterName name).(filterStopWords filterList)) wordList

        -- Return most used word
        return $ snd $ maximum $ occurrences filteredWords


{-|
    Scrapes Wikipedia page with given URL.
    Returns Strings containing article body and CSS tags for filtering.
-}
articleBody :: URL -> MaybeT IO (String, String)
articleBody url = MaybeT $ do
    let
        article :: Scraper String (String, String)
        article = do 
            content <- text ("div" @: ["id" @= "mw-content-text"])
            css <- texts "style"
            return (content, concat css)

        catchAny :: IO a -> (SomeException -> IO a) -> IO a
        catchAny = Control.Exception.catch

    catchAny (scrapeURL url article) (\_ -> return Nothing)


{-|
    Removes punctuation from a string (including hyphens)
    by replacing the characters with spaces.
-}
removePunct :: String -> String
removePunct s = let
    replace :: Char -> Char
    replace x
        | x `elem` (['a'..'z'] ++ ['A'..'Z']  ++ ['0'..'9'] :: String) = x
        | otherwise = ' '
    in
        replace <$> s


{-| 
    Reads stopwords from a file.
    Assumes that the file exists, as it is provided with the spec.
    Will throw an exception otherwise.
-}
getStopWords :: MaybeT IO ([String])
getStopWords = MaybeT $ do  
    contents <- readFile "stopwords.txt"
    stopWords <- return (splitOn "\n" contents)
    if length stopWords < 1
        then return Nothing
        else return $ Just stopWords


{-| 
    Removes stopwords from a list of Strings.
    Takes list of stopwords, list to be filtered.
-}
filterStopWords :: [String] -> [String] -> [String]
filterStopWords stopWords l = let
    stopWordsDict = Map.fromList [(word, 0) | word <- stopWords]
    in
        filter (\x -> (not (x `Map.member` stopWordsDict))) l


{-| 
    Removes items from a list of Strings,
    that start with the same 4 characters as a given String.
-}
filterName :: String -> [String] -> [String]
filterName name l = filter (\x -> not (take 4 x == take 4 name)) l


{-| 
    Determines whether a String is entirely numerical.
-}
isNumerical :: String -> Bool
isNumerical w = foldr (\x acc -> (x `elem` ['0'..'9']) && acc) True w


{-| 
    Removes all numerical tokens from a list of Strings.
-}
filterNumerical :: [String] -> [String]
filterNumerical l = filter (not.isNumerical) l


{-| 
    Counts item occurrences in a list.
    Returns list of tuples of occurrences and items.
-}
occurrences :: (Ord a) => [a] -> [(Int, a)]
occurrences xs = map swap $ Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])
