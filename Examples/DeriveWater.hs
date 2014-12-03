module Examples.DeriveWater where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Derivative
import Accelemation.Animations


import Prelude hiding (subtract)

main :: IO ()
main = generateHtml $ 
    let displacement = derivativeGrayscale waves `multiply` (\t x y -> rgba 0.01 0.01 0.01 1)
    in bendSpaceTime displacement (scale 0.1 0.1 chess)



test1 = derivativeGrayscale grayBall `addition` (\t x y -> rgba 0.5 0.5 0.5 1)
test2 = derivativeGrayscale grayBall `addition` fromGrayscale grayBall
test3 = bendSpaceTime (fastForward 0.1 (orbit 1 1.3 (spin 1 (derivativeGrayscale grayBall)))) (scale 0.04 0.04 chess)


waves :: Grayscale
waves t x y =
    let wave frequence amplitude =
            let d = curry2 magnitude x y
            in sin (frequence * (t + 3 / d)) * amplitude
    in wave 10 1

grayBall :: Grayscale
grayBall _ x y =
    let d = curry2 magnitude x y
    in gaussianNormalized 0.3 d

