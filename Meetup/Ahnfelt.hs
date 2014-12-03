module Meetup.Ahnfelt where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Derivative
import Accelemation.Animations

main :: IO ()
main = generateHtml $ test1


test1 = circelingBall `multiply` bendSpaceTime smallerWave' (scale 0.2 0.2 chess `multiply` rainbow)
test2 = circelingBall `multiply` bendSpaceTime smallerWave' (rotatingChess `multiply` rainbow)


rotatingChess = spin 0.5 (scale 0.2 0.2 chess)
scalingRotatingChess t = scale (sin t / 2 + 1) (sin t / 2 + 1) (rotatingChess) t
circelingBall = circle 1 (gaussBall 0.4)
wave t x y = sin (x * 2) 
wave' = derivativeGrayscale wave
smallerWave' = scroll 1 0 $ wave' `multiply` (\t x y -> rgba 0.1 0.1 0.1 1)


rainbow :: Animation
rainbow _ x _ = hsva (mod' x 1) 0.5 0.5 1

grayBall :: Grayscale
grayBall _ x y =
    distance 0 x 0 y >- \d ->
    gaussianNormalized 0.4 d
