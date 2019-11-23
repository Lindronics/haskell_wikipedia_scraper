{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.HTML.Scalpel

import Text.LaTeX
import Text.LaTeX.Packages.Inputenc
import Data.Matrix

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
     execLaTeXT tables >>= renderFile "tex/tables.tex"

tables :: Monad m => LaTeXT m ()
tables = thePreamble >> document theBody

thePreamble :: Monad m => LaTeXT m ()
thePreamble = do
    documentclass [] article
    usepackage [utf8] inputenc
    author "Daniel Díaz"
    title "Examples of Tables"

theBody :: Monad m => LaTeXT m ()
theBody = do
    maketitle
    -- Table from a simple matrix
    center $ matrixTabular (fmap textbf ["x","y","z"]) $ fromList 3 3 [ (1 :: Int)..]
    -- Table from a matrix calculated in-place
    center $ matrixTabular (fmap textbf ["Number","Square root"]) $ matrix 9 2 $ \(i,j) -> if j == 1 then I i else R $ sqrt $ fromIntegral i

data Number = R Double | I Int

instance Texy Number where
    texy (R x) = texy x
    texy (I i) = texy i