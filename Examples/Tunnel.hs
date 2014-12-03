module Examples.Tunnel where


import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic

main :: IO ()
main = generateHtml $ spin 0.1 hsvDisk

hsvDisk :: Animation
hsvDisk t x y =
    curry2 magnitude x y >- \d ->
    let a1 = acos(x/d)
        a = if' (y .>. 0) a1 (2*pi - a1)
        h = a / (2*pi)
    in hsva h (cap (sigfade (d * (2 + sin t)))) (cap (1 - sigfade d)) 1


cap x = max' 0 (min' 1 x)

