module Accelemation.Arithmetic where

import Accelemation.Language (R, (>-))

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
