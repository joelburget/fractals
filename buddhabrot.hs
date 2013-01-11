{-# LANGUAGE BangPatterns #-}
import qualified Graphics.Gloss.Raster.Field as G
import qualified Graphics.Gloss              as G
import qualified Data.Array.Accelerate       as A
import Data.Complex
import Debug.Trace

mandel :: Int -> Complex Float -> Int
mandel max_depth c = loop 0 0
    where
    loop i !z
        | i == max_depth   = i
        | magnitude z >= 2 = i
        | otherwise        = loop (i+1) (z*z + c)

fractal :: G.Point -> G.Color
fractal (!x, !y) = G.greyN brightness'
    where
        point = x :+ y
        max_depth = 1000
        b = mandel max_depth point
        brightness = (fromIntegral b) / (fromIntegral max_depth)
        brightness' = sqrt $ sqrt brightness

dim :: Int
dim = 1000

pic :: G.Picture
pic = G.makePicture dim dim 1 1 fractal

main :: IO ()
main = G.display (G.InWindow "Fractal" (dim, dim) (10, 10)) (G.makeColor 0 0 0 0) pic
