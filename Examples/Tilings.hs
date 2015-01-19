module Examples.Tilings where

import Accelemation.Language
import Accelemation.Combinators
import Accelemation.Arithmetic
import Accelemation.Animations

import Control.Applicative
import Prelude hiding (subtract)

main :: IO ()
main = generateHtml $ translate 0.4 0.21 $ scaleUniform 0.1 $ squareTiling a

squareTiling :: (R -> R -> Animation) -> Animation
squareTiling f t x y =
    let squareX = floor' (x + 0.5)
        squareY = floor' (y + 0.5)
    in (scaleUniform 0.5 (f squareX squareY)) t (x - squareX) (y - squareY)

a sx sy = (\t x y -> hsva (random ((sx + 13) * (sy + 117) + sx + sy)) 0.5 0.5 1) `multiply` (gaussBall 0.6)

random x = (x * 1337 * sin x `mod'` 17) / 17