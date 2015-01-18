module Accelemation.Arithmetic where

import Accelemation.Language (R, (>-), Term)

gaussian :: Floating a => a -> a -> a
gaussian variance x =
    1 / sqrt(variance) * sqrt(2*pi)*exp(-(x*x)/(2*variance)**2)

gaussianNormalized :: R -> R -> R
gaussianNormalized variance x =
    gaussian variance x / (gaussian variance 0)

sigmoid :: R -> R
sigmoid x = 1 / (1 + exp (-x))

sigfade :: R -> R
sigfade x = sigmoid ((x - 0.5)*10)

sinNormalized :: R -> R
sinNormalized x = sin (x - pi/2) * 0.5 + 0.5

fromPolar :: (R -> R -> Term a) -> (R -> R -> Term a)
fromPolar f x y =
    sqrt (x**2 + y**2) >- \r ->
    atan2' x y >- \phi ->
    f r phi

atan2' :: R -> R -> R
atan2' x y = 2 * atan(y / (sqrt (x**2 + y**2) + x))