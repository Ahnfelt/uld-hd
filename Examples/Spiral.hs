module Examples.Spiral where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Animations

import Control.Applicative
import Prelude hiding (subtract)

main :: IO ()
main = generateHtml $ translate 0.4 0.21 $ zoomy $ fromPolarCoordinates spiral


zoomy :: Animation -> Animation
zoomy f t = scaleUniform (sinNormalized t) f t

spiral :: Animation
spiral t r phi =
    if' (phi .<. 0)  (2*pi + phi) phi >- \positivePhi ->
    positivePhi / (2*pi) >- \unitAngle ->
    let
        d = floor' (r + unitAngle) - unitAngle + 0.5
        h = d / 77
        s = 0.5
        v = gaussianNormalized 0.03 (r - d)
    in hsva h s v 1
