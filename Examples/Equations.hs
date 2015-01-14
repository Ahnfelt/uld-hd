module Examples.Equations where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Animations

import Control.Applicative
import Prelude hiding (subtract)

main :: IO ()
main = generateHtml $ translate 0.4 0.21 $ scaleUniform 0.2 $ additions [
        fromFunction 0.0 f,
        fromEquation 0.1 f2,
        fromEquation 0.2 unitCircle,
        fromEquation 0.3 (fromPolar godenSpiral),
        fromFunction 0.4 sin,
        fromFunction 0.5 cos,
        fromFunction 0.6 sinNormalized,
        fromFunction 0.7 (gaussian 1),
        fromFunction 0.8 (gaussianNormalized 1),
        fromFunction 0.9 sigmoid,
        fromFunction 0.95 sigfade
    ]


additions :: [Animation] -> Animation
additions fs = foldl1 addition fs


-- Functions

-- f(x) = x^2
f :: R -> R
f x = x**2

fromFunction :: R -> (R -> R) -> Animation
fromFunction h f t x y =
    let y' = f x
        error = y' - y
        value = gaussianNormalized 0.01 error
    in hsva h 0.5 value 1


-- Equations

f2 :: R -> R -> R
f2 x y = y + x**2

unitCircle x y = (x**2 + y**2) - 1

fromEquation :: R -> (R -> R -> R) -> Animation
fromEquation h f t x y =
    let error = f x y
        value = gaussianNormalized 0.01 error
    in hsva h 0.5 value 1

godenSpiral r phi = r - a * exp (b * phi) where
    a = 1
    b = 0.30649 --1.618
