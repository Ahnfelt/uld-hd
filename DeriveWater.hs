module Main where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Derivative

import Prelude hiding (subtract)

main :: IO ()
main = generateHtml $ derivativeGrayscale (wave 0 0) `addition` (\t x y -> rgba 0.5 0.5 0.5 1)

test1 = derivativeGrayscale gaussBall `addition` (\t x y -> rgba 0.5 0.5 0.5 1)
test2 = derivativeGrayscale gaussBall `addition` fromGrayscale gaussBall
test3 = bendSpaceTime (fastForward 0.1 (orbit 1 1.3 (spin 1 (derivativeGrayscale gaussBall)))) (scale 0.04 0.04 chess)


waves :: Animation
waves = fromGrayscale $ wave 0 0

wave :: R -> R -> Grayscale
wave centerX centerY t x y =
    let d1 = distance 0 x 0 y
        d2 = distance 1 x 1 y
    in sin (t * 10 + d1 * 10) * 0.1 +
        sin (t * 7 + d2 * 12) * 0.1

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


