
module Meetup.Presentation where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Derivative
import Accelemation.Animations

main :: IO ()
main = generateHtml $ fromGrayscale (grayBall )

grayBall :: R -> Grayscale
grayBall variance _ x y =
    distance 0 x 0 y >- \d ->
    gaussianNormalized variance d