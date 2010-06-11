module WavWrite where

import qualified Data.ByteString.Lazy as B
import Data.Binary.Put
import System.IO
import Data.Word

import qualified Data.ByteString.Lazy.Char8 as BE

channels = 2
samplerate = 44100


writeWavHeader :: Int -> Put
writeWavHeader n = do
    putLazyByteString $ BE.pack "RIFF"
    putWord32le $ fromIntegral (4 + 24 + 8 + n * 2 * channels)
    putLazyByteString $ BE.pack "WAVE"
    putLazyByteString $ BE.pack "fmt "
    putWord32le 16
    putWord16le 1
    putWord16le $ fromIntegral channels
    putWord32le $ fromIntegral samplerate
    putWord32le $ fromIntegral (2 * samplerate * channels)
    putWord16le $ fromIntegral (2 * channels)
    putWord16le 16
    putLazyByteString $ BE.pack "data"
    putWord32le $ fromIntegral (n * 2 * channels)

convertSample :: Double -> Word16
convertSample x
    | x' < 0 = 0xffff + x'
    | otherwise = x'
    where
        x' = round $ 32767 * x

writeWavData :: [Double] -> Put
writeWavData (x:[]) = do
    putWord16le $ convertSample x
    putWord16le $ convertSample x
writeWavData (x:xs) = do
    putWord16le $ convertSample x
    putWord16le $ convertSample x
    writeWavData xs
    

writeWav filename samples = do
    h <- openBinaryFile filename WriteMode
    B.hPut h $ runPut (writeWavHeader (length samples))
    B.hPut h $ runPut (writeWavData samples)
    hClose h
