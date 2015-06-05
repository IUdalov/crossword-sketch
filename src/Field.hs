module Field (Field, Placement(Full), fieldFromFile, pretty, extractAllPatterns, extractPattern, emplaceWord) where

import qualified Data.Matrix as M
import qualified Data.List as List
import Data.Monoid
import Data.Foldable (fold)
import Data.Char (isAlpha, isSpace)

-- data -----------------------------------------------------------------------

-- Row rownum bcol ecol | Col colnum brow erow
type Field = M.Matrix Char
data Placement = Full | Row Int Int Int | Col Int Int Int
    deriving (Show, Eq)

instance Ord Placement where
    f `compare` l = getInd f `compare` getInd l

getInd :: Placement -> (Int, Int, Int)
getInd (Row n f l) = (n, f, l)
getInd (Col n f l) = (n, f, l)

-- load -----------------------------------------------------------------------
fieldFromFile :: String -> IO Field
fieldFromFile filename = do
    content <- readFile filename
    return . toField . lines $ content

toField :: [String] -> Field
toField ls = fmap convertSymbol . M.fromLists . (map $ expandString maxlen) $ ls
    where
        maxlen = foldl max 0 (map length ls)
        expandString num str = str ++ replicate (num - length str) ' '
        convertSymbol s | isAlpha s = s
                        | isSpace s = ' '
                        | otherwise = '.'

-- output ---------------------------------------------------------------------
delimetr :: String
delimetr = "\n----------------\n"

pretty :: [Field] -> String
pretty inp = ( concatMatrs $ map ( concatMatr .  M.toLists) inp ) ++ "\n"
    where
        concatMatr = foldl1 (\ res str-> res ++ "\n" ++ str)
        concatMatrs = foldl1 (\ res matr -> res ++ delimetr ++ matr)

-- extract --------------------------------------------------------------------
extractPattern :: Field -> Placement -> String
extractPattern f (Row r c1 c2) = M.toList $ M.submatrix r r c1 c2 f
extractPattern f (Col c r1 r2) = M.toList $ M.submatrix r1 r2 c c f

extractAllPatterns :: Field -> [Placement]
extractAllPatterns f = mixPlacements rowLoc colLoc
    where
        rowLoc = getRowPlacements f
        colLoc = getColPlacements f

mixPlacements :: [Placement] -> [Placement] -> [Placement]
mixPlacements x y = List.sort (x ++ y)

getRowPlacements :: Field -> [Placement]
getRowPlacements f = concat $ zipWith toRows [1..] plc
    where
        strs = map (\i -> extractPattern f (Row i 1 (M.ncols f))) [1 .. M.nrows f]
        plc = map getPosInStr strs

toRows :: Int -> [(Int, Int)] -> [Placement]
toRows n = map (\(f, s) -> Row n f s)

getColPlacements :: Field -> [Placement]
getColPlacements f = concat $ zipWith toCols [1..] plc
    where
        strs = map (\i -> extractPattern f (Col i 1 (M.nrows f))) [1 .. M.ncols f]
        plc = map getPosInStr strs

toCols :: Int -> [(Int, Int)] -> [Placement]
toCols n = map (\(f, s) -> Col n f s)

-- extract patterns from string -----------------------------------------------
getPosInStr :: String -> [(Int, Int)]
getPosInStr = extractBorders . symToPos

symToPos :: String -> [Int]
symToPos str = zipWith (\ n c -> if c == ' ' then 0 else n) [1..] str

extractBorders :: [Int] -> [(Int, Int)]
extractBorders inp = filter (\(f,s) -> if s < f then False else True) $ map (\(f, s) -> (f + 1, s - 1)) predicted_pos
    where
        zeros_pos = map (+1) $ List.elemIndices 0 inp
        predicted_pos = zip (0:zeros_pos) (zeros_pos ++ [length inp + 1])

-- emplace --------------------------------------------------------------------
emplaceWord :: Field -> Placement -> String -> Field
emplaceWord f (Row r c _) w = foldl insertInRow f [c .. c + len - 1]
    where
        len = length w
        insertInRow fi num = M.setElem (w !! (num - c)) (r, num) fi
emplaceWord f (Col c r _) w = foldl insertInCol f [r .. r + len - 1]
    where
        len = length w
        insertInCol fi num = M.setElem (w !! (num - r)) (num, c) fi

-- TEST -----------------------------------------------------------------------
f :: IO Field
f = fieldFromFile "../templates/lambda_big.iw_template"

ff :: IO Field
ff = fieldFromFile "../templates/test.iw_template"

row :: IO Field
row = fieldFromFile "../templates/lambda_row.iw_template"

col :: IO Field
col = fieldFromFile "../templates/lambda_col.iw_template"