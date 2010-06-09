
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
TokenParser{ naturalOrFloat = m_naturalOrFloat,
             commaSep = m_commaSep } = makeTokenParser langDef

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

argumentList :: Parser [(String,Wavestream)]
argumentList = m_commaSep argument

argument :: Parser (String, Wavestream)
argument = do
                name <- many1 letter;
                char '=';
                val <- expr;
                return (name,val);

lookupArgument :: [(String,Wavestream)] -> String -> Either String Wavestream
lookupArgument [] _ = Left "no such argument"
lookupArgument ((a,v):x) k
    | a == k = Right v
    | otherwise = lookupArgument x k

funcStream :: Parser Wavestream
funcStream = do
                name <- many1 letter;
                char '{';
                arglist <- argumentList
                char '}';
                return $ resolveFunc name (lookupArgument arglist)

resolveFunc :: String -> (String->Either String Wavestream) -> Wavestream
resolveFunc name arg
    | name == "sine" = (NormalWavestream normalSine freq 0.0)
    | name == "sawtooth" = (NormalWavestream normalSawtooth freq 0.0)
    | name == "square" = (NormalWavestream normalSquare freq 0.0)
        where
            freq = case arg "freq" of
                     Left _ -> ConstantWavestream 1.0
                     Right y -> y

bracketed :: Parser Wavestream
bracketed =     do {char '('; x <- expr; char ')'; return x;}
            <|> do {char '['; x <- expr; char ']'; return x;}
            <?> "bracketed expression"

operatorPlus = (+)

main =
    case (parse expr "" "sine{freq=sine{freq=0.3}*9+10.0}") of
        Left err -> do putStr "parse error at "
                       print err
        Right x -> debugShowWavestream x 10000
