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
    section "Status"
    fromString status
    section "Execution Results"
    let l_l = fmap ((fmap pack).pairToList) l
    let matr = fromLists l_l
    center $ matrixTabular (fmap textbf ["Country", "Most frequent word"]) matr
    section "Report"
    fromString report


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
