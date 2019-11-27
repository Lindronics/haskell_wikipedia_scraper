{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Text.HTML.Scalpel

import Text.LaTeX
import Text.LaTeX.Packages.Inputenc
import Data.Matrix
import Data.Text (pack)
import Control.Monad.Trans.Maybe
import Control.Exception

import WikiScrapeLib

countries :: [String]
countries = [
          "Scotland",
          "England",
          "United_Kingdom",
          "USA",
          "Brazil",
          "France",
          "Germany",
          "Italy",
          "Japan",
          "China",
          "Russia"
         ]

wikify :: String -> URL
wikify x = "https://en.wikipedia.org/wiki/" ++ x

main :: IO ()
main = do
    words <- mapM mostfrequentwordonpage (wikify <$> countries)
    let results = zip countries words
    mapM_ (\x -> putStrLn $ (fst x) ++ ": " ++ ((show.snd) x)) results
    status <- getReport "status.txt"
    report <- getReport "report.txt"
    execLaTeXT (tables results status report) >>= renderFile "tex/tables.tex"

tables :: Monad m => [(String, Maybe String)] -> String -> String -> LaTeXT m ()
tables l status report = thePreamble >> document (theBody l status report)

thePreamble :: Monad m => LaTeXT m ()
thePreamble = do
    documentclass [] article
    usepackage [utf8] inputenc
    author "Niklas Lindorfer"
    title "Haskell: Most frequent words on each country's Wikipedia page"

theBody :: Monad m => [(String, Maybe String)] -> String -> String -> LaTeXT m ()
theBody l status report = do
    maketitle
    section "Status"
    fromString status
    section "Execution Results"
    let l_l = fmap ((fmap pack).pairToList) l
    let matr = fromLists l_l
    center $ matrixTabular (fmap textbf ["Country", "Most frequent word"]) matr
    section "Report"
    fromString report


pairToList :: (String, Maybe String) -> [String]
pairToList (x,y) = case y of
    Nothing -> [x, "Nothing"]
    Just y -> [x, y]

getReport :: String -> IO String
getReport s = do
    contents <- try $ readFile s
    case contents of
        Left (e :: SomeException) -> return "*File could not be loaded*"
        Right content -> return content
