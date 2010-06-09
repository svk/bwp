-- The normal wave functions simply evaluate
-- waves on [0,1], expected to be one whole
-- cycle with f(0) = f(1).

import Data.Fixed

normalSine :: (Ord a, Floating a) => a -> a
normalSine x | x >= 0.0 && x <= 1.0 = sin ( x * 2 * pi )

normalSquare :: (Ord a, Floating a) => a -> a
normalSquare x | x >= 0.0 && x < 0.5 = 1.0
               | x >= 0.5 && x <= 1.0 = 0.0

normalSawtooth :: (Ord a, Floating a) => a -> a
normalSawtooth x | x >= 0.0 && x < 0.25 = 4 * x
                 | x >= 0.25 && x < 0.75 = 1 - 4 * (x-0.25)
                 | x >= 0.75 && x <= 1.0 = 4 * (x-0.75) - 1

class Wavestream a where
    sample :: a -> Double
    advance :: Double -> a -> a

data WsConstant = ConstantWavestream Double

instance Wavestream WsConstant where
    sample (ConstantWavestream z) = z
    advance dt a = a

-- NormalWavestream is used to convert a standard periodic
-- function to a wavestream. Note that the phase arg should
-- always be in [0,1].
data WsNormal = NormalWavestream (Double -> Double) Double Double

instance Wavestream WsNormal where
    sample (NormalWavestream f a t) = f t
    advance dt (NormalWavestream f a t) = NormalWavestream f a t'
        where
            t' = (t + a * dt) `Data.Fixed.mod'` 1.0


debugShowWavestream x n
    | n > 0 = do
        putStrLn $ show $ sample $ x
        debugShowWavestream (advance 0.01 x) (n-1)
    | n <= 0  = do
        putStrLn $ show $ sample $ x

main = do
    debugShowWavestream (NormalWavestream normalSine 1.0 0.0) 200
