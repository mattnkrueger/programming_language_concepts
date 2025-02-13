import Prelude
import Data.Char (isLetter, toUpper, chr, ord)

-- Gaurds example
-- this is a function that categorizes a number into one of three categories
categorizeNumber :: Int -> String
categorizeNumber x
  | x < 0     = "Negative"
  | x == 0    = "Zero"
  | x > 0     = "Positive"

-- this is a function that categorizes a number into one of three categories using 'where'
categorizeNumberWhere :: Int -> String
categorizeNumberWhere x = category
  where
    category
      | x < 0     = "Negative"
      | x == 0    = "Zero"
      | x > 0     = "Positive"

-- this is a function that modifies a character based on an integer input
modifyChar :: Int -> Char -> Char
modifyChar n c
  | n < 0     = toLower c
  | n == 0    = c
  | n > 0     = toUpper c
  where
    toLower ch
      | 'A' <= ch && ch <= 'Z' = toEnum (fromEnum ch + 32)
      | otherwise              = ch
    toUpper ch
      | 'a' <= ch && ch <= 'z' = toEnum (fromEnum ch - 32)
      | otherwise              = ch

-- Cypher example from class 
-- uses nested where clauses

-- first where:
--  - shift: shifts a character by n
-- second where:
--  - wrap: the character that is the wrap around point
ceasar :: Int -> String -> String
ceasar n = map (shift n)
    where shift :: Int -> Char -> Char
          shift n c 
            | not (isLetter c) = c
            | toUpper c > wrap = chr (ord c - 26 + n) 
            | otherwise        = chr (ord c + n)
            where wrap :: Char 
                  wrap = chr (ord 'Z' - n)

main = return ()
