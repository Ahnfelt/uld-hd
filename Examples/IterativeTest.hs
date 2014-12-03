module IterativeTest where

import Accelemation.Language
import Accelemation.Combinators

main :: IO ()
main = generateHtml $ \t x y -> iterateWhile 0.1 (\x -> x + 0.1) (\x -> x .<. 0.5) >- \i -> rgba i i i 1

