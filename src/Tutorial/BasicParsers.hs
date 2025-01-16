-- src/Tutorial/BasicParsers.hs
module Tutorial.BasicParsers
    ( parseDigit
    , parseNumber
    , parseWord
    , parseSentence
    , parseCSV
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)

-- Exercise 1: Parse a single digit
-- Hint: Use 'digit' from Parsec
parseDigit :: Parser Char
parseDigit = undefined

-- Exercise 2: Parse a number (multiple digits)
-- Hint: Use 'many1' with parseDigit
parseNumber :: Parser String
parseNumber = undefined

-- Exercise 3: Parse a word (sequence of letters)
-- Hint: Use 'letter' and 'many1'
parseWord :: Parser String
parseWord = undefined

-- Exercise 4: Parse a sentence (sequence of words separated by spaces)
-- Hint: Use 'parseWord' and 'spaces'
parseSentence :: Parser [String]
parseSentence = undefined

-- Exercise 5: Parse CSV (comma-separated values)
-- Each line should contain words separated by commas
-- Hint: Use 'parseWord', 'char' for comma, and 'sepBy'
parseCSV :: Parser [[String]]
parseCSV = undefined
