module Dictionary (Dictionary, SubDictionary, dictFromFile, getByPattern, unixDict) where

import qualified Data.Map.Lazy as M
import Data.Char (isPrint, toLower)

type SubDictionary = [String]
type Dictionary = M.Map Int SubDictionary

-- common ---------------------------------------------------------------------
ind :: Dictionary -> Int -> SubDictionary
ind d n = if M.member n d then d M.! n else []

normalizeWord :: String -> String
normalizeWord = (map toLower) . (filter isPrint)

-- load dictionary ------------------------------------------------------------
dictFromFile :: String -> IO Dictionary
dictFromFile filename = do
    content <- readFile filename
    return $ foldl insertToDict M.empty  $ map normalizeWord (lines content)

insertToDict :: Dictionary -> String -> Dictionary
insertToDict dict word = M.insert len ( (normalizeWord word) : dict `ind` len) dict
    where len = length word

-- searching ------------------------------------------------------------------
-- patern [..ske.l] haskell
getByPattern :: Dictionary -> String -> [String]
getByPattern dict pattern = filter (matchWord pattern) (dict `ind` pos)
    where pos = length pattern

matchWord :: String -> String -> Bool
matchWord pattern word = foldl1 (\ a b -> a && b) $ zipWith isEqual pattern word
    where isEqual a b = a == b || a == '.'

-- other ----------------------------------------------------------------------
unixDict :: IO Dictionary
unixDict = dictFromFile "/usr/share/dict/words"
