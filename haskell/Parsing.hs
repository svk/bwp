
module Main where

import Wavestream

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

foobar x y = x + y

-- We'll start out with integer-based expressions
-- evaluating to integers, then convert to tree
-- structures to evaluate with delays.


expr :: Parser Wavestream
expr = buildExpressionParser tableOperators term
    <?> "expression"

tableOperators = [
                  [Infix (do {string "*"; return ProductWavestream} ) AssocLeft],
                  [Infix (do {string "+"; return SumWavestream} ) AssocLeft]
                 ]

term = number <|> bracketed <?> "basic expression"

number :: Parser Wavestream
number = do
            ds <- many1 digit
            return $ ConstantWavestream $ read ds
         <?> "number"

bracketed :: Parser Wavestream
bracketed =     do {char '('; x <- expr; char ')'; return x;}
            <|> do {char '['; x <- expr; char ']'; return x;}

operatorPlus = (+)

main =
    case (parse expr "" "2*3") of
        Left err -> do putStr "parse error at "
                       print err
        Right x -> debugShowWavestream x 200
