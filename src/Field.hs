module Field (Field, fieldFromFile) where

import qualified Data.Matrix as M

type Field = M.Matrix Char

-- symbols --------------------------------------------------------------------
emptySpace :: Char
emptySpace = '#'

quest :: Char
quest = '?'

-- load -----------------------------------------------------------------------
fieldFromFile :: String -> IO Field
fieldFromFile filename = do
    content <- readFile filename
    return . toField . lines $ content

toField :: [String] -> Field
toField ls = M.fromLists . (map $ expandString maxlen) $ ls
    where
        maxlen = foldl max 0 (map length ls)
        expandString num str = str ++ replicate (num - length str) emptySpace
