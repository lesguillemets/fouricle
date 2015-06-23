import Control.Monad
import Control.Applicative
import Data.IORef
import qualified Data.Map as M
import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Foreign

import Consts

drawCurrent :: Canvas -> Canvas -> Fourier -> Angle -> IO ()
drawCurrent c0 c1 fs θ = do
    -- draw c0
    p <- newIORef ((0,0) :: Point)
    refresh c0
    forM_ (zip [1..] (map (*radius) fs)) $ \ (n,a) -> do
        (nx,ny) <- readIORef p
        let dx = if (floor (n+0.4) `mod` 4) > 1
                     then -a * cos (n*θ)
                     else a * cos (n*θ)
            dy = a * sin (n*θ)
        renderOnTop c0 . toCenter . stroke $ circle (nx,ny) (abs a)
        renderOnTop c0 . toCenter . color blue . stroke $ line (nx,ny) (nx+dx,ny+dy)
        modifyIORef' p (\(x,y) -> (x+dx, y+dy))
    (x,y) <- readIORef p
    renderOnTop c0 . toCenter . color red . stroke $
            line (x,y) (fromIntegral $ barLength-width `div` 2,y)
    -- c1
    shiftPrevious c1 (fromIntegral dw)
    renderOnTop c1 . toOrigin . color green . fill $ circle (0,y) pointSize

mainLoop :: Canvas -> Canvas -> IORef Angle -> IORef Fourier ->
            IORef Angle -> IO ()
mainLoop c0 c1 dθref fsRef θref = do
    fs <- readIORef fsRef
    θ <- readIORef θref
    drawCurrent c0 c1 fs θ
    dθ' <- readIORef dθref
    let θ' = θ + dθ'
        nextθ = if θ' > 2*pi then θ' - 2*pi else θ'
    writeIORef θref nextθ
    setTimeout stepTime (mainLoop c0 c1 dθref fsRef θref)

refresh :: Canvas -> IO ()
refresh c = render c . stroke $ circle (0,0) 0

centerPoint :: Point
centerPoint = (fromIntegral $ width `div` 2 , fromIntegral $ height `div` 2)
toCenter :: Picture () -> Picture ()
toCenter = translate centerPoint

graphOrigin :: Point
graphOrigin = (fromIntegral barLength, fromIntegral $ height `div` 2)
toOrigin :: Picture ()  -> Picture ()
toOrigin = translate graphOrigin

shiftPrevious :: Canvas -> Double -> IO ()
-- TODO : nicer implementation
shiftPrevious = ffi . toJSString . unwords $
        [
        "(function(c,dx){",
            "var buf = document.createElement('canvas');",
            "buf.width = c.width; buf.height=c.height;",
            "buf.getContext('2d').drawImage(c,0,0);",
            "c.width = c.width;",
            "c.getContext('2d').drawImage(buf,dx,0);",
        "})"
        ]

setFs :: IORef Fourier -> IO ()
setFs fsRef = do
    Just series <- elemById "series"
    Just nth <- elemById "nth"
    sName <- getProp series "value"
    n <- read <$> getProp nth "value"
    writeIORef fsRef . take n $ fouriers M.! sName

setUp :: IORef Fourier -> IORef Angle -> IO ()
setUp fsRef dθref = do
    Just series <- elemById "series"
    Just nth <- elemById "nth"
    setFs fsRef
    _ <- onEvent series OnChange (setFs fsRef)
    _ <- onEvent nth OnChange  (setFs fsRef)
    Just speed <- elemById "speed"
    newdθ <- (*dθ) . read <$> getProp speed "value"
    _ <- onEvent speed OnChange $ do
        newdθ <- (*dθ) . read <$> getProp speed "value"
        writeIORef dθref newdθ
    return ()

main = do
    θref <- newIORef 1
    fsRef <- newIORef []
    dθref <- newIORef dθ
    setUp fsRef dθref
    Just canv0 <- getCanvasById "canv0"
    Just canv1 <- getCanvasById "canv1"
    mainLoop canv0 canv1 dθref fsRef θref
