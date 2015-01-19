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
flowers = (flower `addition` timeTravel 1337 flower) `addition` timeTravel 133 flower

flower :: Animation
flower t r phi =
    let
        n = (floor' (t / (2*pi) + 1 `mod'` 10))
        d = sinNormalized (t + phi*n) * (sinNormalized t)
        h = t / 77
        s = d
        v = gaussianNormalized 0.05 (r - d)
    in hsva h s v 1


