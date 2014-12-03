
module Meetup.Presentation where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Derivative
import Accelemation.Animations

main :: IO ()
main = generateHtml $ bendSpaceTime (derivativeGrayscale grayBall) (scale 0.04 0.04 chess) 

--derivativeGrayscale grayBall `addition` (\_ _ _ -> rgba 0.5 0.5 0.5 1)

grayBall :: Grayscale
grayBall _ x y =
    let d = curry2 magnitude x y
    in gaussianNormalized 0.4 d
