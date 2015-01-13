module Examples.Polar where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Animations

import Control.Applicative
import Prelude hiding (subtract)

main :: IO ()
main = generateHtml $ fromPolarCoordinates flowers

flowers :: Animation
flowers = flower `addition` timeTravel 1337 flower

flower :: Animation
flower t r phi =
    let
        n = (floor' (t / (2*pi) + 1 `mod'` 10))
        d = sinNormalized (t + phi*n) * (sinNormalized t) + sinNormalized t * 0.5
        h = t / 77-- phi/(2*pi)
        s = d
        v = gaussianNormalized 0.05 (r - d)
    in hsva h s v 1 -- rgba i 0.1 0.2 i

fromPolarCoordinates :: Animation -> Animation
fromPolarCoordinates f t x y =
    let r = sqrt (x**2 + y**2) in
    let phi = atan2' x y in
    f t r phi

atan2' :: R -> R -> R
atan2' x y = 2 * atan(y / (sqrt (x**2 + y**2) + x))