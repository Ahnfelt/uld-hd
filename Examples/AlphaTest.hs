module Examples.AlphaTest where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic

import Control.Applicative
import Prelude hiding (subtract)

main :: IO ()
main = generateHtml $ alphaSquare `top` (scale 0.05 0.05 chess)

alphaSquare _ x y = rgba (cap x) (cap y) (cap (x * y)) (cap (x * y))
    where cap v = (max' 0 (min' 1 v))


chess :: Animation
chess _ x y =
    floor' (x `mod'` 2) >- \intensityX ->
    floor' (y `mod'` 2) >- \intensityY ->
    abs (intensityY - intensityX) >- \intensity ->
    rgba intensity intensity intensity 1