module IterativeTest where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Animations

main :: IO ()
main = generateHtml $ \t ->
    let a = scale (2**(t*0.5)) (2**(t*0.5)) $ translate (-0.743643887037158704752191506114774) 0.131825904205311970493132056385139 mandelbrot
    in a t
