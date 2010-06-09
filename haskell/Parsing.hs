
module Main where

import Wavestream

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

langDef = emptyDef{ commentLine = "#",
                    identLetter = alphaNum,
                    opStart = oneOf "+*",
                    identStart = letter,
                    reservedOpNames = ["+", "*"],
                    opLetter = oneOf "+*",
                    reservedNames = [] }
TokenParser{ naturalOrFloat = m_naturalOrFloat } = makeTokenParser langDef

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

term = constantStream <|> funcStream <|> bracketed <?> "basic expression"

constantStream :: Parser Wavestream
constantStream = do { ds <- m_naturalOrFloat;
                      case ds of
                        Left n -> return $ ConstantWavestream $ fromIntegral $ n
                        Right x -> return $ ConstantWavestream $ x
                    }
                 <?> "constant stream"

funcStream :: Parser Wavestream
funcStream = do
                name <- many1 letter;
                char '{';
                char '}';
                return $ resolveFunc name

resolveFunc :: String -> Wavestream
resolveFunc name
    | name == "sine" = (NormalWavestream normalSine 1.0 0.0)
    | name == "sawtooth" = (NormalWavestream normalSawtooth 1.0 0.0)
    | name == "square" = (NormalWavestream normalSquare 1.0 0.0)

bracketed :: Parser Wavestream
bracketed =     do {char '('; x <- expr; char ')'; return x;}
            <|> do {char '['; x <- expr; char ']'; return x;}
            <?> "bracketed expression"

operatorPlus = (+)

main =
    case (parse expr "" "1+0.2*sine{}") of
        Left err -> do putStr "parse error at "
                       print err
        Right x -> debugShowWavestream x 200
