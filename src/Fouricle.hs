import Control.Monad
import Data.IORef
import Haste
import Haste.DOM
import Haste.Graphics.Canvas
type Fourier = [Double]

drawCurrent :: Canvas -> Fourier -> IORef (Picture ()) -> Angle -> IO ()
drawCurrent c0 fs graph θ = do
    p <- newIORef ((0,0) :: Point)
    render c0 . stroke $ circle (0,0) 0
    forM_ (zip [1..] (map (*radius) fs)) $ \ (n,a) -> do
        (nx,ny) <- readIORef p
        let θ' = if a < 0 then θ+pi else θ
        let dx = a * cos (n*θ)
            dy = a * sin (n*θ)
        renderOnTop c0 . translate centerPoint . stroke $ circle (nx,ny) (abs a)
        renderOnTop c0 . translate centerPoint . color blue . stroke $ line (nx,ny) (nx+dx,ny+dy)
        modifyIORef' p (\(x,y) -> (x+dx, y+dy))
    return ()

mainLoop :: Canvas -> Fourier -> IORef Angle -> IO ()
mainLoop c0 fs θref = do
    θ <- readIORef θref
    graph <- newIORef (stroke $ rect (0,0) (0,0))
    drawCurrent c0 fs graph θ
    let θ' = θ + dθ
        nextθ = if θ' > 2*pi then θ' - 2*pi else θ'
    writeIORef θref nextθ
    print nextθ
    setTimeout 60 (mainLoop c0 fs θref)

fourier :: Fourier
fourier = [((-1)^(n+1))/ fromIntegral n | n <- [1..10]]
dθ :: Angle
dθ = 0.05
radius :: Double
radius = 100
width :: Int
width = 500
height :: Int
height = 500
centerPoint :: Point
centerPoint = (fromIntegral $ width `div` 2 , fromIntegral $ height `div` 2)

blue :: Color
blue = RGB 0 0 250

main = do
    Just canv0 <- getCanvasById "canv"
    θref <- newIORef 1
    mainLoop canv0 fourier θref
