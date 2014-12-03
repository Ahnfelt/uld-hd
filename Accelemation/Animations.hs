module Accelemation.Animations (
    chess, gaussBall
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
