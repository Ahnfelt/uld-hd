module Examples.Nature where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic

main :: IO ()
main = generateHtml $ translate 0 0.5 flower

flower :: Animation
flower = flowerHead `top` stalk

flowerHead :: Animation
flowerHead = translate (-stalkCurve 1) (-1) $ scale 0.4 0.4 $
    alphaBall `multiply` (\t x y -> rgba 0.8 0.2 0.2 1)

stalk :: Animation
stalk _ x y =
    let d = x - stalkCurve y
        g = gaussianNormalized 0.01 d
        c = if' ((0 .<. y) .&&. (y .<. 1)) g 0
    in c >- \i -> rgba 0.2 0.8 0.3 i


stalkCurve y = cos (y * 9) * 0.04 + sin (y * 5) * 0.09

alphaBall :: Animation
alphaBall _ x y =
    curry2 magnitude x y >- \d ->
    gaussianNormalized 0.3 d >- \intensity ->
    rgba 1 1 1 intensity
