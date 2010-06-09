
module Main where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

foobar x y = x + y

-- We'll start out with integer-based expressions
-- evaluating to integers, then convert to tree
-- structures to evaluate with delays.


expr :: Parser Integer
expr = buildExpressionParser tableOperators term
    <?> "expression"

tableOperators = [
                  [Infix (do {string "*"; return (*)} ) AssocLeft],
                  [Infix (do {string "+"; return (+)} ) AssocLeft]
                 ]

term = number
       <?> "basic expression"

number :: Parser Integer
number = do
            ds <- many1 digit
            return $ read ds
         <?> "number"

operatorPlus = (+)

main =
    do
        parseTest expr "4+4*3"
