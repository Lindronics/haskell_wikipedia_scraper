{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.HTML.Scalpel

import Text.LaTeX
import Text.LaTeX.Packages.Inputenc
import Data.Matrix
import Data.Text (pack)

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
     execLaTeXT (tables results) >>= renderFile "tex/tables.tex"

tables :: Monad m => [(String, Maybe String)] -> LaTeXT m ()
tables l = thePreamble >> document (theBody l)

thePreamble :: Monad m => LaTeXT m ()
thePreamble = do
    documentclass [] article
    usepackage [utf8] inputenc
    author "Niklas Lindorfer"
    title "Haskell: Most frequent words on each country's Wikipedia page"

theBody :: Monad m => [(String, Maybe String)] -> LaTeXT m ()
theBody l = do
    maketitle
    let l_l = fmap ((fmap pack).pairToList) l
    let matr = fromLists l_l
    center $ matrixTabular (fmap textbf ["Country", "Most frequent word"]) matr

pairToList :: (String, Maybe String) -> [String]
pairToList (x,y) = case y of
    Nothing -> [x, "Nothing"]
    Just y -> [x, y]
