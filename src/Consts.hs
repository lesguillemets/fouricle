module Consts where
import Haste.Graphics.Canvas
import qualified Data.Map as M
import Data.Complex

defaultdθ :: Angle
defaultdθ = 0.02
radius :: Double
radius = 100
width :: Int
width = 500
height :: Int
height = 500

red :: Color
red = RGBA 250 0 0 0.6
blue :: Color
blue = RGB 0 0 250
green :: Color
green = RGBA 0 250 0 0.6

barLength :: Int
barLength = 550

stepTime :: Int
stepTime = 30

graphWidth :: Int
graphWidth = 250
dw :: Int
dw = 1
pointSize :: Double
pointSize = 1.0

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

spiral :: Fourier
spiral = [1 / (fromIntegral n*log(fromIntegral n)) | n <- [2..]::[Int]]

play0 :: Fourier
play0 = [ 0.2/ ((-1)^n *log (fromIntegral (n+1))) | n <- [1..] :: [Int]]

play1 :: Fourier
play1 = [if n `mod` 7 == 1
            then  1 / fromIntegral n
            else 0 | n <- [1..] :: [Int]]

fouriers :: M.Map String Fourier
fouriers = M.fromList [
                      ("rectangular", rectWave),
                      ("triangular", triangularWave),
                      ("sawTooth", sawToothWave),
                      ("spiral", spiral),
                      ("leafy", leafy),
                      ("play0", play0),
                      ("play1", play1)
                      ]
