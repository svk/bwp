module Wavestream where

-- The normal wave functions simply evaluate
-- waves on [0,1], expected to be one whole
-- cycle with f(0) = f(1).

import Data.Fixed
import System.Random

normalSine :: (Ord a, Floating a) => a -> a
normalSine x | x >= 0.0 && x <= 1.0 = sin ( x * 2 * pi )
             | otherwise = error "normal argument outside range"

normalSquare :: (Ord a, Floating a) => a -> a
normalSquare x | x >= 0.0 && x < 0.5 = 1.0
               | x >= 0.5 && x <= 1.0 = -1.0
               | otherwise = error "normal argument outside range"

normalSawtooth :: (Ord a, Floating a) => a -> a
normalSawtooth x | x >= 0.0 && x <= 1.0 = 2 * x - 1
                 | otherwise = error "normal argument outside range"

data Wavestream = ConstantWavestream Double
                | NormalWavestream (Double -> Double) Wavestream Double
                | SumWavestream Wavestream Wavestream
                | ProductWavestream Wavestream Wavestream
                | FadeoutWavestream (Double -> Double) Wavestream Double Double
                | FadeinWavestream (Double -> Double) Wavestream Double Double
                | LinearInterpolationWavestream [(Double,Double)] Double Double
                | SpeedShiftWavestream Wavestream Wavestream
                | DelayedWavestream Wavestream Double
                | RandomWavestream StdGen Wavestream Wavestream
                | ClipWavestream Wavestream Wavestream Wavestream

sample :: Wavestream -> Double
sample (ConstantWavestream z) = z
sample (NormalWavestream f _ t) = f t
sample (SumWavestream a b) = (sample a) + (sample b)
sample (ProductWavestream a b) = (sample a) * (sample b)
sample (DelayedWavestream a 0.0) = (sample a)
sample (DelayedWavestream _ _) = 0
sample (FadeoutWavestream f _ t e)
    | rv >= e = rv
    | otherwise = 0
        where
            rv = f t
sample (FadeinWavestream f _ t e)
    | rv < (1-e) = rv
    | otherwise = 1
        where
            rv = f t
sample (LinearInterpolationWavestream ((duration,v1):_) t v0) = (t/duration) * (v1-v0) + v0
sample (LinearInterpolationWavestream [] _ v0) = v0
sample (SpeedShiftWavestream wave _) = sample wave
sample (RandomWavestream g mn mx) = fst $ randomR ((sample mn), (sample mx)) g
sample (ClipWavestream w mn mx)
    | sg > ma = ma
    | sg < mi = mi
    | otherwise = sg
    where
        sg = sample w
        mi = sample mn
        ma = sample mx

advance :: Double -> Wavestream -> Wavestream
advance _ a@(ConstantWavestream _) = a
advance dt (NormalWavestream f a t) = NormalWavestream f a' t'
    where
        a' = (advance dt a)
        t' = (t + (sample a) * dt) `Data.Fixed.mod'` 1.0
advance dt (SumWavestream a b) = SumWavestream (advance dt a) (advance dt b)
advance dt (ProductWavestream a b) = ProductWavestream (advance dt a) (advance dt b)
advance dt (FadeoutWavestream f a t e) = FadeoutWavestream f (advance dt a) (t + (sample a) * dt) e
advance dt (FadeinWavestream f a t e) = FadeinWavestream f (advance dt a) (t + (sample a) * dt) e
advance dt (LinearInterpolationWavestream l@((duration,v1):dvs) t v0)
    | t' >= duration = advance (t' - duration) (LinearInterpolationWavestream dvs 0.0 v1)
    | otherwise = LinearInterpolationWavestream l t' v0
    where
        t' = t + dt
advance dt (LinearInterpolationWavestream [] t v0) = LinearInterpolationWavestream [] (t + dt) v0
advance dt (SpeedShiftWavestream wave a) = SpeedShiftWavestream (advance (dt * (sample a)) wave) (advance dt a)
advance dt (DelayedWavestream a 0.0) = DelayedWavestream (advance dt a) 0.0
advance dt (DelayedWavestream a b) = DelayedWavestream a (max 0.0 (b - dt))
advance dt (RandomWavestream g mn mx) = RandomWavestream (snd $ next g) (advance dt mn) (advance dt mx)
advance dt (ClipWavestream a b c) = ClipWavestream (advance dt a) (advance dt b) (advance dt c)

nil :: Wavestream -> Bool
nil (ConstantWavestream z) = z == 0
nil (SumWavestream a b) = (nil a) && (nil b)
nil (ProductWavestream a b) = (nil a) || (nil b)
nil w@(FadeoutWavestream _ _ _ _) = (sample w) == 0
nil (LinearInterpolationWavestream [] _ v0) = v0 == 0
nil (SpeedShiftWavestream wave _) = nil wave
nil (DelayedWavestream wave 0.0) = nil wave
nil (RandomWavestream _ mn mx) = nil mn && nil mx
nil (ClipWavestream sig mn mx) = (nil mn && nil mx) -- Theoretically correct but somewhat problematic:
                                                    --  a clipped wavestream won't be nil even if the
                                                    --  signal is, since the clips could later force
                                                    --  the output to be non-nil.
                                 || (nil sig && (fixed mn) && (fixed mx) && 0.0 >= (sample mn) && 0.0 <= (sample mx))
                                                    -- The major case that is not handled is clips that
                                                    -- are not constant but have ceilings and floors.
nil _ = False


fixed :: Wavestream -> Bool
fixed (ConstantWavestream _) = True
fixed (SumWavestream a b) = fixed a && fixed b
fixed (ProductWavestream a b) = fixed a && fixed b
fixed x@(FadeoutWavestream _ _ _ _) = sample x == 0
fixed x@(FadeinWavestream _ _ _ _) = sample x == 1
fixed (LinearInterpolationWavestream [] _ _) = True
fixed (SpeedShiftWavestream wave _) = fixed wave
fixed (DelayedWavestream wave 0) = fixed wave
fixed (RandomWavestream _ mn mx) = fixed mn && fixed mx
fixed (ClipWavestream w mn mx) = (fixed mn && fixed mx)
                                 &&
                                 ((sample mn == sample mx) || fixed w)
fixed _ = False

debugShowWavestream :: Wavestream -> Integer -> IO ()

debugShowWavestream x n
    | n > 0 = do
        putStrLn $ show $ sample $ x
        debugShowWavestream (advance 0.01 x) (n-1)
    | otherwise = do
        putStrLn $ show $ sample $ x
