module Fouriers where
import Data.Complex
import qualified Data.Map as M

type Fourier = [Double]
rectWave :: Fourier
rectWave = [if even n then 0 else 1/fromIntegral n | n <- [1..]::[Int]]

triangularWave :: Fourier
triangularWave = [
                 if even n
                     then 0
                     else 1/fromIntegral ((-1)^(n`div`2)*n^2) |
                     n <- [1..]::[Int]
                ]

sawToothWave :: Fourier
sawToothWave = [1 / fromIntegral n | n <- [1..]::[Int]]

leafy :: Fourier
leafy = [imagPart ((0:+1)^(n-1)) / (fromIntegral n* log (fromIntegral n))
            | n <- [2..] :: [Int]]

wavy :: Fourier
wavy = 0:[1 / (fromIntegral n*log(fromIntegral n)) | n <- [2..]::[Int]]

play0 :: Fourier
play0 = 0:[ 0.2/ ((-1)^n *log (fromIntegral (n))) | n <- [2..] :: [Int]]

play1 :: Fourier
play1 = [if n `mod` 7 == 1
            then  1 / fromIntegral n
            else 0 | n <- [1..] :: [Int]]

fouriers :: M.Map String Fourier
fouriers = M.fromList [
                      ("rectangular", rectWave),
                      ("triangular", triangularWave),
                      ("sawTooth", sawToothWave),
                      ("wavy", wavy),
                      ("leafy", leafy),
                      ("play0", play0),
                      ("play1", play1)
                      ]
