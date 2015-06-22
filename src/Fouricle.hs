import Control.Monad
import Data.IORef
import Haste
import Haste.DOM
import Haste.Graphics.Canvas

import Consts
type Fourier = [Double]

drawCurrent :: Canvas -> Canvas ->
                Fourier -> IORef (Picture ()) -> Angle -> IO ()
drawCurrent c0 c1 fs graph θ = do
    p <- newIORef ((0,0) :: Point)
    refresh c0
    forM_ (zip [1..] (map (*radius) fs)) $ \ (n,a) -> do
        (nx,ny) <- readIORef p
        let dx = a * cos (n*θ)
            dy = a * sin (n*θ)
        renderOnTop c0 . toCenter . stroke $ circle (nx,ny) (abs a)
        renderOnTop c0 . toCenter . color blue . stroke $ line (nx,ny) (nx+dx,ny+dy)
        modifyIORef' p (\(x,y) -> (x+dx, y+dy))
    (x,y) <- readIORef p
    renderOnTop c0 . toCenter . color red . stroke $
            line (x,y) (fromIntegral $ barLength-width `div` 2,y)

mainLoop :: Canvas -> Canvas -> Fourier -> IORef Angle -> IO ()
mainLoop c0 c1 fs θref = do
    θ <- readIORef θref
    graph <- newIORef (stroke $ rect (0,0) (0,0))
    drawCurrent c0 c1 fs graph θ
    let θ' = θ + dθ
        nextθ = if θ' > 2*pi then θ' - 2*pi else θ'
    writeIORef θref nextθ
    setTimeout stepTime (mainLoop c0 c1 fs θref)

refresh :: Canvas -> IO ()
refresh c = render c . stroke $ circle (0,0) 0

fourier :: Fourier
fourier = [if even n then 0 else 1/fromIntegral n | n <- [1..50]]

centerPoint :: Point
centerPoint = (fromIntegral $ width `div` 2 , fromIntegral $ height `div` 2)

toCenter :: Picture () -> Picture ()
toCenter = translate centerPoint

main = do
    print $ fourier
    Just canv0 <- getCanvasById "canv0"
    Just canv1 <- getCanvasById "canv1"
    θref <- newIORef 1
    mainLoop canv0 canv1 fourier θref
