module Tutorial.JSON
    ( JSONValue(..)
    , parseJSON
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Map as Map

data JSONValue = JNull
               | JBool Bool
               | JNumber Double
               | JString String
               | JArray [JSONValue]
               | JObject (Map.Map String JSONValue)
               deriving (Show, Eq)

-- Exercise 7: Parse JSON
-- Should handle null, booleans, numbers, strings, arrays, and objects
-- Hint: Use recursive parsers for nested structures
parseJSON :: Parser JSONValue
parseJSON = undefined
