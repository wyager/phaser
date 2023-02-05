module Lib
    ( run2,
    World(..)
    ) where

import qualified Graphics.Gloss as  G
import Graphics.Gloss.Data.ViewPort (ViewPort)
import qualified Graphics.Gloss.Raster.Array as GR
import qualified Data.Array.Repa as R
import Data.Complex (Complex((:+)), polar)
import Data.Colour.RGBSpace.HSV (hsv)
import Data.Colour.RGBSpace (RGB(..))

--run :: IO ()
--run = G.simulate 
--    (G.InWindow "Phaser" (600,600) (10,10))
--    G.black
--    300 -- steps per second
--    (World 0 150 1)
--    render
--    step
--    
--
--data World = World {p :: Double, x :: Double, m :: Double}
--
--render :: World ->  G.Picture
--render (World _p x _m) = G.Color G.white $ G.Circle (realToFrac x)
--
--step :: ViewPort -> Float -> World -> World
--step _ dtF (World p x m) = World p' x' m
--    where
--    dt = realToFrac dtF
--    x' = x + (p / m) * dt
--    p'= p + f * dt
--    f = - (x - 100)
      

data World = World (R.Array R.U R.DIM1 (Complex Double))

run2 :: IO ()
run2 = GR.playArray 
    (G.InWindow "Phaser" (600,600) (10,10))
    (1,100) -- NB: Adjust aspect ratio here
    100
    world0
    render
    (\_evt world -> world)
    (\_dt world -> world)

world0 :: World
world0 = World $ R.fromListUnboxed (R.ix1 100) $ take 100 $ iterate spin (1 :+ 0)
    where
    spin x = exp (0 :+ 0.3) * x

render :: World -> R.Array R.D R.DIM2 G.Color
render (World phase) = R.reshape (R.ix2 1 100) $ R.map c2c phase

c2c :: Complex Double -> G.Color
c2c cpx = GR.rgb r g b 
    where 
    (mag, phase) = polar (fmap realToFrac cpx)
    threesixty = (phase + pi) * (360 / (2 * pi))
    RGB r g b = hsv threesixty 1 1 
