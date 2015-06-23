module Consts where
import Haste.Graphics.Canvas

dθ :: Angle
dθ = 0.02
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
rectWave = [if even n then 0 else 1/fromIntegral n | n <- [1..]]

triangularWave :: Fourier
triangularWave = [
                 if even n
                     then 0
                     else 1/fromIntegral ((-1)^(n`div`2)*n^2) | n <- [1..]]