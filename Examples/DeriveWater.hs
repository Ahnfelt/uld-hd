module Examples.DeriveWater where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Derivative

import Prelude hiding (subtract)

main :: IO ()
main = generateHtml $ derivativeGrayscale waves `addition` (\t x y -> rgba 0.5 0.5 0.5 1)



test1 = derivativeGrayscale gaussBall `addition` (\t x y -> rgba 0.5 0.5 0.5 1)
test2 = derivativeGrayscale gaussBall `addition` fromGrayscale gaussBall
test3 = bendSpaceTime (fastForward 0.1 (orbit 1 1.3 (spin 1 (derivativeGrayscale gaussBall)))) (scale 0.04 0.04 chess)


water :: Animation
water = fromGrayscale waves `addition` (\t x y -> rgba 0.5 0.5 0.5 1)

waves :: Grayscale
waves t x y =
    let f a = distance (cos a) x (sin a) y
        d1 = f (pi/2)
        d2 = f (pi/2 + 2*pi/3)
        d3 = f (0)
    in 
        sin (-t * 12 + d1 * 12) * 0.5 * 0.3 +
        sin (-t * 10 + d2 * 10) * 0.5 * 0.3 +
        sin (-t * 8 + d3 * 8) * 0.5 * 0.3 

gaussBall :: Grayscale
gaussBall _ x y =
    let d = distance 0 x 0 y
    in gaussianNormalized 0.3 d

chess :: Animation
chess _ x y =
    floor' (x `mod'` 2) >- \intensityX ->
    floor' (y `mod'` 2) >- \intensityY ->
    abs (intensityY - intensityX) >- \intensity ->
    rgba intensity intensity intensity 1


