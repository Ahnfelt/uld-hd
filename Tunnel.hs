module Main where


import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic

import Control.Applicative
import Prelude hiding (subtract)

main :: IO ()
main = generateHtml $ hsvDisk

hsvDisk :: Animation
hsvDisk t x y =
    distance 0 x 0 y >- \d ->
    let a1 = acos(x/d)
        a = if' (y .>. 0) a1 (2*pi - a1)
        h = a / (2*pi)
    in hsva h (cap (sigfade (d * 2))) (cap (1 - sigfade d)) 1


cap x = max' 0 (min' 1 x)
