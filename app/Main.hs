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
import Data.List.Split (splitOn)

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


{-| 
    Runs mostfrequentwordonpage on the list of countries.
    Outputs the results to stdout.
    Generates a .tex file containing the tabulated results
    and the contents of the report and status files.
-}
main :: IO ()
main = do
    words <- mapM mostfrequentwordonpage (wikify <$> countries)
    let results = zip countries words
    mapM_ (\x -> putStrLn $ (fst x) ++ ": " ++ ((show.snd) x)) results
    status <- getReport "status.txt"
    report <- getReport "report.txt"
    execLaTeXT (texFile results status report) >>= renderFile "tex/tables.tex"


{-| 
    Generates the contents of the LaTeX file.
    Takes list of results, status file contents, report contents.
-}
texFile :: Monad m => [(String, Maybe String)] -> String -> String -> LaTeXT m ()
texFile l status report = thePreamble >> document (theBody l status report)


{-| 
    Generates the preamble for the LaTeX file.
-}
thePreamble :: Monad m => LaTeXT m ()
thePreamble = do
    documentclass [] article
    usepackage [utf8] inputenc
    author "Niklas Lindorfer"
    title "Haskell: Most frequent words on each country's Wikipedia page"


{-| 
    Generates the body for the LaTeX file.
    Takes list of results, status file contents, report contents.
-}
theBody :: Monad m => [(String, Maybe String)] -> String -> String -> LaTeXT m ()
theBody l status report = do
    maketitle

    -- Status: triple newlines denote new section
    section "Status"
    fromStrList (splitOn "\n\n\n" status) False

    -- Get execution results
    section "Execution Results"
    let matr = fromLists (fmap ((fmap pack).pairToList) l)
    center $ matrixTabular (fmap textbf ["Country", "Most frequent word"]) matr

    -- Report: triple newlines denote new section
    section "Report"
    fromStrList (splitOn "\n\n\n" report) False


{-| 
    Generates a section for the LaTeX file.
    Takes a list of Strings.
    Alternates between headings and body.
-}
fromStrList :: Monad m => [String] -> Bool -> LaTeXT_ m
fromStrList [] _ = ""
fromStrList (x:xs) isHeading = do
    if isHeading
        then subsection $ fromString x
        else fromString x
    fromStrList xs (not isHeading)


{-| 
    Converts output list to flattened list of Strings.
    This is necessary to handle maybe values.
-}
pairToList :: (String, Maybe String) -> [String]
pairToList (x,y) = case y of
    Nothing -> [x, "N/A"]
    Just y -> [x, y]


{-| 
    Retrieves a file given a path.
    Will return placeholder string if an exception occurs.
-}
getReport :: String -> IO String
getReport s = do
    contents <- try $ readFile s
    case contents of
        Left (e :: SomeException) -> return "*File could not be loaded*"
        Right content -> return content
