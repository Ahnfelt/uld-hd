module Accelemation.Arithmetic where

import Accelemation.Language (R, (>-))

gaussian :: Floating a => a -> a -> a
gaussian variance x =
    1 / sqrt(variance) * sqrt(2*pi)*exp(-(x*x)/(2*variance)**2)

gaussianNormalized :: R -> R -> R
gaussianNormalized variance x =
    gaussian variance 0 >- \maxValue ->
    gaussian variance x / maxValue

distance :: Floating a => a -> a -> a -> a -> a
distance x1 x2 y1 y2 = sqrt ((x1 - x2)**2 + (y1 - y2)**2)