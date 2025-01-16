-- src/Tutorial/Calculator.hs
module Tutorial.Calculator
    ( Expr(..)
    , parseExpr
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)

data Expr = Number Int
         | Add Expr Expr
         | Sub Expr Expr
         | Mul Expr Expr
         | Div Expr Expr
         deriving (Show, Eq)

-- Exercise 6: Parse a simple calculator expression
-- Should handle basic arithmetic (+, -, *, /) with proper precedence
-- Hint: Use chainl1 for operator precedence
parseExpr :: Parser Expr
parseExpr = undefined
