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
                | FadeoutWavestream (Double -> Double) Wavestream Double Double
                | FadeinWavestream (Double -> Double) Wavestream Double

sample :: Wavestream -> Double
sample (ConstantWavestream z) = z
sample (NormalWavestream f a t) = f t
sample (SumWavestream a b) = (sample a) + (sample b)
sample (ProductWavestream a b) = (sample a) * (sample b)
sample (FadeoutWavestream f a t e)
    | rv >= e = rv
    | otherwise = 0
        where
            rv = f t
sample (FadeinWavestream f a t)
    | rv < 1 = rv
    | otherwise = 1
        where
            rv = f t

advance :: Double -> Wavestream -> Wavestream
advance dt a@(ConstantWavestream _) = a
advance dt (NormalWavestream f a t) = NormalWavestream f a' t'
    where
        a' = (advance dt a)
        t' = (t + (sample a) * dt) `Data.Fixed.mod'` 1.0
advance dt (SumWavestream a b) = SumWavestream (advance dt a) (advance dt b)
advance dt (ProductWavestream a b) = ProductWavestream (advance dt a) (advance dt b)
advance dt (FadeoutWavestream f a t e) = FadeoutWavestream f (advance dt a) (t + (sample a) * dt) e
advance dt (FadeinWavestream f a t) = FadeinWavestream f (advance dt a) (t + (sample a) * dt)

nil :: Wavestream -> Bool
nil (ConstantWavestream z) = z == 0
nil (SumWavestream a b) = (nil a) && (nil b)
nil (ProductWavestream a b) = (nil a) || (nil b)
nil w@(FadeoutWavestream f a t e) = (sample w) == 0
nil _ = False

debugShowWavestream x n
    | n > 0 = do
        putStrLn $ show $ sample $ x
        debugShowWavestream (advance 0.001 x) (n-1)
    | n <= 0  = do
        putStrLn $ show $ sample $ x

-- main = do
--    debugShowWavestream (NormalWavestream normalSine 1.0 0.0) 200
