module Utils (Args, parseArgs, printHelp)
where

import System.Environment

data Args = Args {
    template_path :: String,
    dictionary_path :: String,
    is_help :: Bool
} deriving (Show)

parseArgs :: IO Args
parseArgs = undefined

printHelp :: IO ()
printHelp = do
    prog <- getProgName
    putStr $ "Usage: " ++ prog ++ " [-h] | [-i template -d dictionary]"
