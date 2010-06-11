
module Main where

import Wavestream
import List
import WavWrite

import System (getArgs)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

langDef = emptyDef{ commentLine = "#",
                    opStart = oneOf "+*",
                    identStart = letter <|> char '_',
                    identLetter = alphaNum <|> char '_',
                    reservedOpNames = ["+", "*", ":", ";"],
                    opLetter = oneOf "+*:;",
                    reservedNames = [] }
TokenParser{ naturalOrFloat = m_naturalOrFloat,
             commaSep = m_commaSep,
             commaSep1 = m_commaSep1,
             whiteSpace = m_whiteSpace,
             reservedOp = m_reservedOp,
             identifier = m_identifier,
             symbol = m_symbol } = makeTokenParser langDef

-- We'll start out with integer-based expressions
-- evaluating to integers, then convert to tree
-- structures to evaluate with delays.
--
--

type WaveBindings = [(String,ScriptType)]

expr :: GenParser Char WaveBindings ScriptType
expr = buildExpressionParser tableOperators term
    <?> "expression"

opProduct (WavestreamType a) (WavestreamType b) = WavestreamType $ ProductWavestream a b
opProduct (PartialWavestreamType (WaveFunctionPartial _ s _)) _ = error ("left argument to product missing arguments: " ++ show s)
opProduct _ (PartialWavestreamType (WaveFunctionPartial _ s _)) = error ("right argument to product missing arguments: " ++ show s)

opSum (WavestreamType a) (WavestreamType b) = WavestreamType $ SumWavestream a b
opSum (PartialWavestreamType (WaveFunctionPartial _ s _)) _ = error ("left argument to sum missing arguments: " ++ show s)
opSum _ (PartialWavestreamType (WaveFunctionPartial _ s _)) = error ("right argument to sum missing arguments: " ++ show s)

tableOperators = [
                  [Infix (do {m_reservedOp "*"; return opProduct} ) AssocLeft],
                  [Infix (do {m_reservedOp "+"; return opSum} ) AssocLeft]
                 ]

term = constantStream <|> try (funcStream) <|> namedWave <|> pairList <|> bracketed <?> "basic expression"

pairList :: GenParser Char WaveBindings ScriptType
pairList = do
    m_symbol "[";
    valuelist <- m_commaSep numberPair;
    m_symbol "]";
    return (PairListType valuelist)


constantStream :: GenParser Char WaveBindings ScriptType
constantStream = do { ds <- m_naturalOrFloat;
                      case ds of
                        Left n -> return $ WavestreamType $ ConstantWavestream $ fromIntegral $ n
                        Right x -> return $ WavestreamType $ ConstantWavestream $ x
                    }
                 <?> "constant stream"

argumentList :: GenParser Char WaveBindings [(String,ScriptType)]
argumentList = m_commaSep argument

data ScriptType = WavestreamType Wavestream
                  | PairListType [(Double,Double)]
                  | PartialWavestreamType WaveFunctionPartial

toDouble :: (Either Integer Double) -> Double
toDouble (Left n) = fromIntegral n
toDouble (Right x) = x

namedWave :: GenParser Char WaveBindings ScriptType
namedWave = do
                s <- m_identifier;
                st <- getState;
                case (lookupBinding st s) of
                    Left msg -> fail msg
                    Right (WavestreamType wave) -> return (WavestreamType wave)

numberPair :: GenParser Char WaveBindings (Double, Double)
numberPair = do
                m_symbol "(";
                x <- m_naturalOrFloat;
                m_symbol ",";
                y <- m_naturalOrFloat;
                m_symbol ")";
                return (toDouble x, toDouble y)


argument :: GenParser Char WaveBindings (String, ScriptType)
argument = try (do
                name <- m_identifier;
                m_symbol "=";
                val <- expr;
                return (name,val);)

addBinding :: String -> ScriptType -> [(String,ScriptType)] -> [(String, ScriptType)]
addBinding s w l = ((s,w):l)

lookupBinding :: [(String,ScriptType)] -> String -> Either String ScriptType
lookupBinding [] s = Left ("no such binding: " ++ s)
lookupBinding ((a,v):x) k
    | a == k = Right v
    | otherwise = lookupBinding x k

lookupArgument :: [(String,ScriptType)] -> String -> Either String ScriptType
lookupArgument [] _ = Left "no such argument"
lookupArgument ((a,v):x) k
    | a == k = Right v
    | otherwise = lookupArgument x k

funcStream :: GenParser Char WaveBindings ScriptType
funcStream = do
                name <- m_identifier;
                m_symbol "{";
                arglist <- argumentList
                m_symbol "}";
                st <- getState
                case (findFunc name st) of
                    Left s -> fail s
                    Right x ->
                        case (evaluateFop $ resolveFop arglist x) of
                            Left _ -> return (PartialWavestreamType x)
                            Right result -> return (WavestreamType result)

type WaveFunction = [(String,ScriptType)] -> Wavestream
data WaveFunctionPartial = WaveFunctionPartial WaveFunction [String] [(String,ScriptType)]

wsArg :: [(String,ScriptType)] -> String -> Wavestream
wsArg ((n,(WavestreamType v)):nvs) s
    | n == s = v
    | otherwise = wsArg nvs s
wsArg [] s = error ("no such argument: " ++ s)

daArg :: [(String,ScriptType)] -> String -> [(Double,Double)]
daArg ((n,(PairListType v)):nvs) s
    | n == s = v
    | otherwise = daArg nvs s
daArg [] s = error ("no such argument: " ++ s)

findFunc :: String -> WaveBindings -> Either String WaveFunctionPartial
findFunc "sine" _ = Right $ WaveFunctionPartial (\a -> NormalWavestream normalSine (wsArg a "freq") 0.0) ["freq"] []
findFunc "sawtooth" _ = Right $ WaveFunctionPartial (\a -> NormalWavestream normalSawtooth (wsArg a "freq") 0.0) ["freq"] []
findFunc "triangular" _ = Right $ WaveFunctionPartial (\a -> (SpeedShiftWavestream (LinearInterpolationWavestream (cycle [(0.25,1.0),(0.5,-1.0),(0.25,0.0)]) 0.0 0.0) (wsArg a "freq"))) ["freq"] []
findFunc "square" _ = Right $ WaveFunctionPartial (\a -> NormalWavestream normalSquare (wsArg a "freq") 0.0) ["freq"] []
findFunc "expdecay" _ = Right $ WaveFunctionPartial (\a -> (FadeoutWavestream (\x -> exp (-x)) (wsArg a "speed") 0.0 0.001)) ["speed"] []
findFunc "lineardecay" _ = Right $ WaveFunctionPartial (\a -> (FadeoutWavestream (\x -> 1 - x) (wsArg a "speed") 0.0 0.0)) ["speed"] []
findFunc "linearinterpolation" _ = Right $ WaveFunctionPartial (\a -> (LinearInterpolationWavestream (daArg a "data") 0.0 (sample (wsArg a "initial")))) ["data", "initial"] []
findFunc name bindings = case (lookupBinding bindings name) of
                            Right (PartialWavestreamType x) -> Right x
                            _ -> Left name

resolveFop :: [(String,ScriptType)] -> WaveFunctionPartial -> WaveFunctionPartial
resolveFop ((an,av):as) (WaveFunctionPartial f ua ra) = resolveFop as $ WaveFunctionPartial f (delete an ua) ((an,av):ra)
resolveFop [] x = x

evaluateFop :: WaveFunctionPartial -> Either [String] Wavestream
evaluateFop (WaveFunctionPartial f [] a) = Right $ f a
evaluateFop (WaveFunctionPartial _ s _) = Left s

{-
resolveFunc :: String -> (String->Either String ScriptType) -> Wavestream
resolveFunc name arg
    | name == "sine" = (NormalWavestream normalSine freq 0.0)
    | name == "sawtooth" = (NormalWavestream normalSawtooth freq 0.0)
    | name == "triangular" = (SpeedShiftWavestream (LinearInterpolationWavestream (cycle [(0.25,1.0),(0.5,-1.0),(0.25,0.0)]) 0.0 0.0) freq)
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
-}
                    

bracketed :: GenParser Char WaveBindings ScriptType
bracketed =     do
                    m_symbol "(";
                    x <- expr;
                    m_symbol ")";
                    return x;
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

exportWavestreamFrom t wave dt maxTime
    | (nil wave) || (t > maxTime) = []
    | otherwise = ((sample wave):exportWavestreamFrom (t + dt) (advance dt wave) dt maxTime)

outputWavestream = outputWavestreamFrom 0
exportWavestream = exportWavestreamFrom 0

coolsound = "sine{freq=triangular{freq=1.0}*100.0+440.0}*expdecay{speed=3.0}*(0.3*sine{freq=40*lineardecay{speed=0.5}}+0.7)*lineardecay{speed=2.0}"
simplesound = "sine{freq=440}*linearinterpolation{data=[(0.1,1.0),(0.05,0.7),(0.2,0.7),(0.1,0.0)]}"
coolsound' = "sine{freq=sawtooth{freq=1.0}*100.0+440.0}*expdecay{speed=3.0}*(0.3*sine{freq=40*lineardecay{speed=0.5}}+0.7)*lineardecay{speed=2.0}"

debugcontext = [ ("mysine",
                  (NormalWavestream normalSine (ConstantWavestream 10.0) 0.0) )
            ]
usingctx = "sine{freq=440}*(mysine*0.5+0.5)*linearinterpolation{data=[(0.1,1.0),(0.05,0.7),(0.2,0.7),(0.1,0.0)]}"

myctx = "alpha:sine{freq=10}*0.5+0.5;output:sine{freq=440}*alpha;"

parseLater :: [String] -> WaveBindings -> [Char] -> ScriptType
parseLater s b t = PartialWavestreamType $ WaveFunctionPartial (\a -> case (runParser expr (b++a) "" t) of
                                                                           Right (WavestreamType x) -> x
                                                                           _ -> error "error in delayed parse") s []

bindingValue :: GenParser Char WaveBindings ScriptType
bindingValue = do
                    m_symbol ":";
                    y <- expr;
                    m_symbol ";";
                    return y;
               <|> do
                    m_symbol "{";
                    names <- m_commaSep1 m_identifier
                    m_symbol "}";
                    m_symbol ":";
                    statenow <- getState
                    unparsed <- manyTill anyChar (try (m_symbol ";"))
                    return $ parseLater names statenow unparsed

fullParser :: GenParser Char WaveBindings WaveBindings
fullParser = m_whiteSpace >> many (do
                x <- m_identifier;
                y <- bindingValue;
                updateState (addBinding x y)
                return (x,y))

exportFromFile :: String -> String -> Integer -> Double -> IO (Either String [Double])
exportFromFile wavename filename samplerate maxtime = do
    input <- readFile filename;
    case (runParser fullParser [] "" input) of
        Left err -> return $ Left $ show err
        Right waves -> case (lookupBinding waves wavename) of
            Right (WavestreamType wave) -> return $ Right $ exportWavestream wave (1.0/(fromIntegral samplerate)) maxtime
            Left err -> return $ Left ("no such wave: " ++ wavename)

main = do
        cmdargs <- getArgs
        let inputFile = cmdargs !! 0
            waveName = cmdargs !! 1
            outputFile = cmdargs !! 2
            maxtime = 3.0
            samplerate = 44100
            in do putStrLn ("Writing wave \"" ++ waveName ++ "\" from file \"" ++ inputFile ++ "\" to WAV \"" ++ outputFile ++ "\".")
                  putStrLn ("Sample rate: " ++ show samplerate)
                  putStrLn ("Maximum duration: " ++ (show maxtime) ++ " seconds")
                  result <- exportFromFile waveName inputFile samplerate maxtime
                  case result of
                      Left err -> do putStr "error: "
                                     putStrLn err
                      Right samples -> do putStrLn "Writing to WAV file."
                                          writeWav outputFile samples
                                          putStrLn "Done!"
