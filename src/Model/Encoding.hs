module Model.Encoding where

import Data.Char (toLower)
import Data.Function (on)

-- Because of IRC's Scandinavian origin, the characters {}|^ are
-- considered to be the lower case equivalents of the characters []\~,
-- respectively.
toLower' :: Char -> Char
toLower' '{' = '['
toLower' '}' = ']'
toLower' '|' = '\\'
toLower' '^' = '~'
toLower'  c  = toLower c

-- Case-insensitive comparison of Chars, with the caveat that
-- Because of IRC's Scandinavian origin, the characters {}|^ are
-- considered to be the lower case equivalents of the characters []\~,
-- respectively.
charEquals :: Char -> Char -> Bool
charEquals = (==) `on` toLower'

