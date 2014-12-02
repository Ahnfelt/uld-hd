module Examples.HidingDevils where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Animations

main :: IO ()
main = generateHtml hidingDevils

hidingDevils :: Animation
hidingDevils = chessDevils `multiply` seekingLights

chessDevils :: Animation
chessDevils = bendSpaceTime devilMirror (scale 0.04 0.04 chess)

devilMirror :: Animation
devilMirror =
    hellHole    1  0.2  0.05  2.5   3  2.5  5   `addition`
    hellHole   13  0.1  0.02  3     9  0.5  0.1 `addition`
    hellHole  133  0.4  0.05  2.9   2  1    0.5 `addition`
    hellHole 1337  0.9  0.04  3.1   11 5    0.9

hellHole :: R -> R -> R -> R -> R -> R -> R -> Animation
hellHole startTime size timeFactor orbitX orbitY spinSpeed rotationSpeed =
    timeTravel startTime $ fastForward timeFactor $ circle rotationSpeed $ orbit orbitX orbitY $ spin spinSpeed $ scale size size displacementBall

displacementBall :: Animation
displacementBall t x y =
    distance 0 x 0 y >- \d ->
    gaussianNormalized 0.3 d >- \intensity ->
    rgba (intensity * x / d) (intensity * y / d) 0 1

light :: Color -> Animation
light color = orbit 1 2 (spin 0.2 (scale 0.5 0.6 (gaussBall 0.3))) `multiply` (\t x y -> color)

seekingLights :: Animation
seekingLights =
    fastForward 0.2 (spin 0.2 (timeTravel 1    (light (rgba 0.7 0.2 0.2 1)))) `addition`
    fastForward 0.3 (spin 0.3 (timeTravel 133   (light (rgba 0.2 0.7 0.2 1)))) `addition`
    fastForward 0.4 (spin 0.4 (timeTravel 1337 (light (rgba 0.2 0.2 0.7 1))))


