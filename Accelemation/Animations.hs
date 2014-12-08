module Accelemation.Animations (
    chess, gaussBall, mandelbrot
) where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic

import Control.Applicative
import Prelude hiding (subtract)

chess :: Animation
chess _ x y =
    floor' (x `mod'` 2) >- \intensityX ->
    floor' (y `mod'` 2) >- \intensityY ->
    abs (intensityY - intensityX) >- \intensity ->
    rgba intensity intensity intensity 1
    
gaussBall :: R -> Animation
gaussBall variance _ x y =
    curry2 magnitude x y >- \d ->
    gaussianNormalized variance d >- \intensity ->
    rgba intensity intensity intensity 1

mandelbrot :: Animation
mandelbrot t x y = while >- \i -> rgba i i i 1 where
    while = iterateWhile 100
        (vec2 0 0)
        (\v0 i -> vec2 (getX v0 * getX v0 - getY v0 * getY v0 + x) (2.0 * getX v0 * getY v0 + y))
        (\v i -> magnitude v .>. 2)
        (\v i -> 1 - (i / 100))
