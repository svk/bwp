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
normalSawtooth x = 2 * x - 1

data Wavestream = ConstantWavestream Double
                | NormalWavestream (Double -> Double) Wavestream Double
                | SumWavestream Wavestream Wavestream
                | ProductWavestream Wavestream Wavestream
                | FadeoutWavestream (Double -> Double) Wavestream Double Double
                | FadeinWavestream (Double -> Double) Wavestream Double
                | LinearInterpolationWavestream [(Double,Double)] Double Double
                | SpeedShiftWavestream Wavestream Wavestream

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
sample (LinearInterpolationWavestream ((duration,v1):_) t v0) = (t/duration) * (v1-v0) + v0
sample (LinearInterpolationWavestream [] _ v0) = v0
sample (SpeedShiftWavestream wave _) = sample wave

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
advance dt (LinearInterpolationWavestream l@((duration,v1):dvs) t v0)
    | t' >= duration = advance (t' - duration) (LinearInterpolationWavestream dvs 0.0 v1)
    | otherwise = LinearInterpolationWavestream l t' v0
    where
        t' = t + dt
advance dt (LinearInterpolationWavestream [] t v0) = LinearInterpolationWavestream [] (t + dt) v0
advance dt (SpeedShiftWavestream wave a) = SpeedShiftWavestream (advance (dt * (sample a)) wave) (advance dt a)

nil :: Wavestream -> Bool
nil (ConstantWavestream z) = z == 0
nil (SumWavestream a b) = (nil a) && (nil b)
nil (ProductWavestream a b) = (nil a) || (nil b)
nil w@(FadeoutWavestream f a t e) = (sample w) == 0
nil (LinearInterpolationWavestream [] _ v0) = v0 == 0
nil (SpeedShiftWavestream wave _) = nil wave
nil _ = False

debugShowWavestream x n
    | n > 0 = do
        putStrLn $ show $ sample $ x
        debugShowWavestream (advance 0.01 x) (n-1)
    | n <= 0  = do
        putStrLn $ show $ sample $ x

--main = do
--    debugShowWavestream (LinearInterpolationWavestream [(0.5, 1.0), (0.25, 0.25), (0.75, 0.25), (0.5, 0.0), (1.0, 1.0), (0.1, -0.5)] 0.0 0.0) 2000
