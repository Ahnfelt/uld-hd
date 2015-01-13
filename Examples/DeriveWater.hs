module Examples.DeriveWater where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Derivative
import Accelemation.Animations


import Prelude hiding (subtract)

main :: IO ()
main = generateHtml $ 
    let displacement = derivativeGrayscale waves
    in bendSpaceTime displacement (scale 0.1 0.1 chess)



test1 = derivativeGrayscale grayBall `addition` (\t x y -> rgba 0.5 0.5 0.5 1)
test2 = derivativeGrayscale grayBall `addition` fromGrayscale grayBall
test3 = bendSpaceTime (fastForward 0.1 (orbit 1 1.3 (spin 1 (derivativeGrayscale grayBall)))) (scale 0.04 0.04 chess)


waves :: Grayscale
waves t x y =
    let wave cx cy frequence amplitude =
            let d = sqrt ((x - cx)**2 + (y - cy)**2)
            in sin (frequence * t + d) * amplitude
    in wave 0 0 7 0.01 + wave (-1) (-1) 5 0.01 + wave (-1) (-1) 5 0.1 + wave (1) (-1) 5 0.01 + wave (-0.3) (0.7) 17 0.007

waves2 :: Grayscale
waves2 t x y =
    let wave frequence amplitude = sin (frequence * t + x) * amplitude
    in wave 11 0.01 + wave 3 0.1 + wave 7 0.2 + wave 22 0.05


grayBall :: Grayscale
grayBall _ x y =
    let d = curry2 magnitude x y
    in gaussianNormalized 0.3 d

