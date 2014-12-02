module Examples.DeriveWater where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Derivative

import Prelude hiding (subtract)

main :: IO ()
main = generateHtml $ 
    let displacement = derivativeGrayscale waves `multiply` (\t x y -> rgba 0.01 0.01 0.01 1)
    in bendSpaceTime displacement (scale 0.1 0.1 chess)



test1 = derivativeGrayscale gaussBall `addition` (\t x y -> rgba 0.5 0.5 0.5 1)
test2 = derivativeGrayscale gaussBall `addition` fromGrayscale gaussBall
test3 = bendSpaceTime (fastForward 0.1 (orbit 1 1.3 (spin 1 (derivativeGrayscale gaussBall)))) (scale 0.04 0.04 chess)


waves :: Grayscale
waves t x y =
    let wave centerX centerY frequence speed amplitude =
            let d = distance centerX x centerY y
            in sin (-frequence * t + speed * d) * amplitude
    in wave 0 0 10 10 0.3

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


