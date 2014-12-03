
module Meetup.Presentation where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Derivative
import Accelemation.Animations

main :: IO ()

main = generateHtml $ (\t x y -> (scale (sin  (t*0.1)) 1 bg) t x y)

tial :: Animation -> Animation
tial target t x y = target t ((1-y )*x) y


bg = ( rainbow `addition` (rotate (pi/2) rainbow) `addition`  (hi `addition` (timeTravel 3 hi)))

spinBall :: Animation
spinBall = circle 1 (gaussBall 0.2)


hi = spinBall `addition` spinBall

fff x = mod' (x*x) 1

rainbow :: Animation
rainbow _ x _ = (hsva (mod' x 1)  0.5  (fff (x+x))  1)