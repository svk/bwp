
module Main where

import Wavestream

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

langDef = emptyDef{ commentLine = "#",
                    opStart = oneOf "+*",
                    identStart = letter <|> char '_',
                    identLetter = alphaNum <|> char '_',
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

argumentList :: Parser [(String,ScriptType)]
argumentList = m_commaSep argument

data ScriptType = WavestreamType Wavestream
                  | PairListType [(Double,Double)]

toDouble :: (Either Integer Double) -> Double
toDouble (Left n) = fromIntegral n
toDouble (Right x) = x

numberPair :: Parser (Double, Double)
numberPair = do
                char '(';
                x <- m_naturalOrFloat;
                char ',';
                y <- m_naturalOrFloat;
                char ')';
                return (toDouble x, toDouble y)


argument :: Parser (String, ScriptType)
argument = try (do
                name <- many1 letter;
                char '=';
                val <- expr;
                return (name,WavestreamType val);)
           <|> do
                name <- many1 letter;
                char '=';
                char '[';
                valuelist <- m_commaSep numberPair;
                char ']';
                return (name, PairListType valuelist)

lookupArgument :: [(String,ScriptType)] -> String -> Either String ScriptType
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

resolveFunc :: String -> (String->Either String ScriptType) -> Wavestream
resolveFunc name arg
    | name == "sine" = (NormalWavestream normalSine freq 0.0)
    | name == "sawtooth" = (NormalWavestream normalSawtooth freq 0.0)
    | name == "square" = (NormalWavestream normalSquare freq 0.0)
    | name == "expdecay" = (FadeoutWavestream (\x -> exp (-x)) speed 0.0 0.001)
    | name == "lineardecay" = (FadeoutWavestream (\x -> 1 - x) speed 0.0 0.0)
    | name == "linearinterpolation" = (LinearInterpolationWavestream dataarg 0.0 initial)
        where
            freq = case arg "freq" of
                     Left _ -> ConstantWavestream 1.0
                     Right (WavestreamType wave) -> wave
                     _ -> error "inappropriate type"
            speed = case arg "speed" of
                     Left _ -> ConstantWavestream 1.0
                     Right (WavestreamType wave) -> wave
                     _ -> error "inappropriate type"
            initial = case arg "initial" of
                     Left _ -> 0.0
                     Right (WavestreamType wave) -> (sample wave)
                     _ -> error "inappropriate type"
            dataarg = case arg "data" of
                     Left _ -> error "expected data"
                     Right (PairListType pairlist) -> pairlist
                     _ -> error "inappropriate type"
                    

bracketed :: Parser Wavestream
bracketed =     do {char '('; x <- expr; char ')'; return x;}
            <?> "bracketed expression"

operatorPlus = (+)

outputSample wave t = do
    putStr $ show $ t
    putStr $ " "
    putStrLn $ show $ (sample wave)

outputWavestreamFrom t wave dt maxTime
    | (nil wave) || (t > maxTime) = outputSample wave t
    | otherwise = do
        outputSample wave t
        outputWavestreamFrom (t + dt) (advance dt wave) dt maxTime

outputWavestream = outputWavestreamFrom 0

coolsound = "sine{freq=sawtooth{freq=1.0}*100.0+440.0}*expdecay{speed=3.0}*(0.3*sine{freq=40*lineardecay{speed=0.5}}+0.7)*lineardecay{speed=2.0}"
simplesound = "sine{freq=440}*linearinterpolation{data=[(0.1,1.0),(0.05,0.7),(0.2,0.7),(0.1,0.0)]}"

main =
    case (parse expr "" simplesound) of
        Left err -> do putStr "parse error at "
                       print err
        Right x -> outputWavestream x (1.0/44100.0) 3.0
