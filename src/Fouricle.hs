import Data.IORef
import Haste
import Haste.DOM
import Haste.Graphics.Canvas
type Fourier = [Double]

drawCurrent :: Canvas -> Canvas -> Fourier -> Angle -> IO ()
drawCurrent _ _ [] _ = return ()
drawCurrent c0 c1 (f:fs) θ = undefined


mainLoop :: Canvas -> Canvas -> Fourier -> IORef Angle -> IO ()
mainLoop c0 c1 fs θref = do
    θ <- readIORef θref
    drawCurrent c0 c1 fs θ
    let θ' = θ + dθ
        nextθ = if θ' > pi then θ' - pi else θ
    writeIORef θref nextθ

fourier :: Fourier
fourier = undefined
dθ :: Angle
dθ = 0.1
width :: Int
width = 500
height :: Int
height = 500

canv0 = undefined
canv1 = undefined

main = do
    θref <- newIORef 0
    mainLoop canv0 canv1 fourier θref
