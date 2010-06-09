module Wavestream where

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

data Wavestream = ConstantWavestream Double
                | NormalWavestream (Double -> Double) Wavestream Double
                | SumWavestream Wavestream Wavestream
                | ProductWavestream Wavestream Wavestream

sample :: Wavestream -> Double
sample (ConstantWavestream z) = z
sample (NormalWavestream f a t) = f t
sample (SumWavestream a b) = (sample a) + (sample b)
sample (ProductWavestream a b) = (sample a) * (sample b)

advance :: Double -> Wavestream -> Wavestream
advance dt a@(ConstantWavestream _) = a
advance dt (NormalWavestream f a t) = NormalWavestream f a' t'
    where
        a' = (advance dt a)
        t' = (t + (sample a) * dt) `Data.Fixed.mod'` 1.0
advance dt (SumWavestream a b) = SumWavestream (advance dt a) (advance dt b)
advance dt (ProductWavestream a b) = ProductWavestream (advance dt a) (advance dt b)

debugShowWavestream x n
    | n > 0 = do
        putStrLn $ show $ sample $ x
        debugShowWavestream (advance 0.001 x) (n-1)
    | n <= 0  = do
        putStrLn $ show $ sample $ x

-- main = do
--    debugShowWavestream (NormalWavestream normalSine 1.0 0.0) 200
