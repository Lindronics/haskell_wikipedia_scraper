{-# LANGUAGE OverloadedStrings #-}

module WikiScrapeLib
    (  mostfrequentwordonpage, articleBody, test, removePunct
    ) where

import Text.HTML.Scalpel
import Data.List.Split

test :: IO (Maybe [String])
test = 
    (fmap.fmap.fmap) removePunct ((fmap.fmap) splitWords articleBody)

-- test :: IO (Maybe [String])
-- test = do
--     asdf <- (fmap.fmap) splitWords articleBody
--     x <- removePunct asdf
--     return x


mostfrequentwordonpage :: URL -> IO (Maybe String)
mostfrequentwordonpage page = do
  return (Just "fixme")

type Article = String

articleBody :: IO (Maybe Article)
articleBody = scrapeURL "https://en.wikipedia.org/wiki/Germany" article
    where
        article :: Scraper String Article
        article = text ("div" @: ["id" @= "mw-content-text"])

splitWords :: Article -> [String]
splitWords t = splitOn " " t

removePunct :: String -> String
removePunct s = filter (/= '\n') s

-- getCounts :: [String] -> [String, Int]
-- getCounts 
