{-# LANGUAGE BangPatterns #-}
import Graphics.Gloss.Raster.Field hiding (dim)
import Graphics.Gloss hiding (dim)
import Data.Complex

mandel :: Int -> Complex Float -> Int
mandel max_depth c = loop 0 0
    where
    loop i !z
        | i == max_depth   = i
        | mag3      z >= 2 = i
        | otherwise        = loop (i+1) (z*z + c)
    
    mag3 :: Complex Float -> Float
    mag3 (x:+y) = 
        if      r == 0 then i
        else if i == 0 then r   
        else if r >= i then let q = i / r in r * sqrt(1 + q*q)
        else                let q = r / i in i * sqrt(1 + q*q)
      where r = abs x
            i = abs y

fractal :: Point -> Color
fractal (!x, !y) = greyN brightness'
    where
        point = x :+ y
        max_depth = 1000
        b = mandel max_depth point
        brightness = (fromIntegral b) / (fromIntegral max_depth)
        brightness' = sqrt $ sqrt brightness

dim :: Int
dim = 400

pic :: Picture
pic = makePicture dim dim 1 1 fractal

main :: IO ()
main = display (InWindow "Fractal" (dim, dim) (10, 10)) (makeColor 0 0 0 0) pic
