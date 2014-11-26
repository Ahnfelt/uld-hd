module Main where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic

import Control.Applicative
import Prelude hiding (subtract)

main :: IO ()
main = generateHtml $ timeballs

spinBall :: Animation
spinBall = circle 1 gaussBall

balls :: Animation
balls = spinBall `addition` timeTravel (2*pi/3) spinBall `addition` timeTravel (4*pi/3) spinBall

colorbow :: Animation
colorbow = scroll 1 0 ((scale 10 1 rainbow) `multiply` wave)

rainballs :: Animation
rainballs = colorbow `addition` balls

timebow :: Animation
timebow = bendSpaceTime hole (scroll 1 0 rainbow)

timeballs :: Animation
timeballs = bendSpaceTime hole rainballs

hole = (circle 0.7 gaussBall) `multiply` (\t x y -> rgba 0 0 5 1)


--------------------------------
-- Some time-invariant animations (aka. images)
--------------------------------

rainbow :: Animation
rainbow _ x _ = hsva (mod' x 1) 0.5 0.5 1

wave :: Animation
wave _ x y =
    1 - abs (y - sin (x)) >- \intensity ->
    rgba intensity intensity intensity 1

gaussBall :: Animation
gaussBall _ x y =
    distance 0 x 0 y >- \d ->
    gaussianNormalized 0.3 d >- \intensity ->
    rgba intensity intensity intensity 1
