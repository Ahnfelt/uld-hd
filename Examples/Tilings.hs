module Examples.Tilings where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Animations

import Control.Applicative
import Prelude hiding (subtract)

main :: IO ()
--main = generateHtml $ translate 0.4 0.21 $ useTransformation (toRotatedYCoordinates (-pi/6)) $ scaleUniform 0.1 $ squareTiling a
main = generateHtml $ translate 0.4 0.21 $ scaleUniform 0.1 $ triangleTiling b
--main = generateHtml $ translate 0.4 0.21 $ scaleUniform 0.1 $ blabla2

squareTiling :: (R -> R -> Animation) -> Animation
squareTiling f t x y =
    let squareX = floor' (x + 0.5)
        squareY = floor' (y + 0.5)
    in (scaleUniform 0.5 (f squareX squareY)) t (x - squareX) (y - squareY)

a sx sy = (\t x y -> hsva (random ((sx + 13) * (sy + 117) + sx + sy)) 0.5 0.5 1) `multiply` (gaussBall 0.6)
random x = (x * 1337 * sin x `mod'` 17) / 17

b sx sy = (\t x y -> hsva (sqrt(x**2 + y**2) * sy) 0.5 0.5 1) `multiply` (gaussBall (abs (sx*0.1)))


fromRotatedYCoordinates :: R -> R -> R -> (R, R)
fromRotatedYCoordinates angle x y = (x + y * cos (0.5*pi + angle), y * sin (0.5*pi + angle))

toRotatedYCoordinates :: R -> R -> R -> (R, R)
toRotatedYCoordinates angle x y = (x - y * cos (0.5*pi + angle), y / sin (0.5*pi + angle))

useTransformation :: (R -> R -> (R, R)) -> Animation -> Animation
useTransformation transformation f t x y =
    let (x', y') = transformation x y
    in f t x' y'


triangleTiling :: (R -> R -> Animation) -> Animation
triangleTiling f t x y =
    let
        (x', y') = toRotatedYCoordinates (-pi/6) x y
        centerX' = floor' (x' + 0.5)
        centerY' = floor' (y' + 0.5)
        (x'', y'') = fromRotatedYCoordinates (-pi/6) (centerX' - x') (centerY' - y')
        px = centerX'
        py = centerY' * 2 + floor' ((x' + y') * 2)
    in (scaleUniform 0.5 (f px py)) t x'' y''


blabla :: Animation
blabla t x y =
    let
        squareX = floor' (x + 0.5)
        squareY = floor' (y + 0.5)
        d = min' (abs (squareX - x)) (abs (squareY - y))
        i = (gaussianNormalized 0.02 d)
    in rgba i i i 1

blabla2 :: Animation
blabla2 t x y =
    let
        (x', y') = toRotatedYCoordinates (-pi/6) x y
        squareX' = floor' (x' + 0.5)
        squareY' = floor' (y' + 0.5)
        d = min' (abs (squareX' - x')) (abs (squareY' - y'))
        i = (gaussianNormalized 0.02 d)
        dc = sqrt ((squareX' - x')**2 + (squareY' - y')**2)

        (x'', y'') = fromRotatedYCoordinates (-pi/6) (squareX' - x') (squareY' - y')
        dc' = sqrt (x''**2 + y''**2)
    in rgba i dc' 0 1
