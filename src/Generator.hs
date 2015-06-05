module Generator (generate)
where

import qualified Data.Matrix as M

import Field
import Dictionary

generate :: Dictionary -> Field -> [Field]
generate d f = generate' (extractAllPatterns f) d f

generate' :: [Placement] -> Dictionary -> Field -> [Field]
generate' [] d f = [f]
generate' (p:pls) d f = concat $ map (generate' pls d) fields
    where
        pattern = extractPattern f p
        wrds = getByPattern d pattern
        fields = map (emplaceWord f p) wrds
