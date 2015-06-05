module Main
where

import System.Environment (getArgs)

import Field
import Dictionary
import Generator
import Utils

find_crosswords :: Int -> String -> String -> IO ()
find_crosswords n dict_name template_name = do
    dict <- dictFromFile dict_name
    template <- fieldFromFile template_name
    let result = generate dict template
    putStrLn (show n)
    putStr . pretty . take n $ result
    

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [num, dict_name, template_name] -> find_crosswords (read num) dict_name template_name
        _ -> putStrLn "Usage: <numer of crosswords> <dictionary> <template>"
