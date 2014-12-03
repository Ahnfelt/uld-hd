module Meetup.Oscar where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Derivative
import Accelemation.Animations

main :: IO ()
main = generateHtml $ fromGrayscale testBall

grayBall :: Grayscale
grayBall _ x y =
    let d = curry2 magnitude x y
    in gaussianNormalized 0.4 d
    
testBall :: Grayscale
testBall _ x y =
    let d = curry2 magnitude x y
    in gaussianNormalized 0.4 d




