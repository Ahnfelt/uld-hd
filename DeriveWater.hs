module Main where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Derivative

import Control.Applicative
import Prelude hiding (subtract)

main :: IO ()
main = generateHtml $ test3

test1 = derivativeGrayscale gaussBall `addition` (\t x y -> rgba 0.5 0.5 0.5 1)
test2 = derivativeGrayscale gaussBall `addition` fromGrayscale gaussBall
test3 = bendSpaceTime (fastForward 0.1 (orbit 1 1.3 (spin 1 (derivativeGrayscale gaussBall)))) (scale 0.04 0.04 chess)


gaussBall :: Grayscale
gaussBall _ x y =
    let d = distance 0 x 0 y
    in gaussianNormalized 0.3 d

fromGrayscale f t x y = f t x y >- \i -> rgba i i i 1

chess :: Animation
chess _ x y =
    floor' (x `mod'` 2) >- \intensityX ->
    floor' (y `mod'` 2) >- \intensityY ->
    abs (intensityY - intensityX) >- \intensity ->
    rgba intensity intensity intensity 1


