{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Pixel
import Control.Applicative
import Prelude hiding (subtract)


--------------------------------
-- Write the shader
--------------------------------

main :: IO ()
main = generateHtml $ chessLights


--------------------------------
-- Some funky animations
--------------------------------

circleBall :: Animation
circleBall = orbit 1 2 (spin 0.2 (const (scale 0.5 0.6 gaussBall)))

balls :: Animation
balls = fastForward 0.3 $ spin 0.2 $ circleBall `addition` jump (2*pi/3) circleBall `addition` jump (4*pi/3) circleBall

colorbow :: Animation
colorbow = scroll 1 0 (const (scale 10 1 rainbow) `multiply` const wave)

rainballs :: Animation
rainballs = colorbow `addition` balls

timebow :: Animation
timebow = bendSpaceTime hole (scroll 1 0 (const rainbow))

timeballs :: Animation
timeballs = bendSpaceTime hole rainballs

hole = (circle 0.7 (const gaussBall)) `multiply` (\t x y -> rgba 0 0 5 1)

chessLights = chessDevils `multiply` (const (scale 10 1 rainbow)) `multiply` balls

chessDevils :: Animation = bendSpaceTime devilMirror (const $ scale 0.04 0.04 chess)
    where
        devilMirror =
            hellHole    1  0.2  0.05  2.5   3  2.5  5 `addition`
            hellHole   13  0.1  0.02  3     9  0.5  0.1 `addition`
            hellHole  133  0.4  0.05  2.9   2  1    0.5 `addition`
            hellHole 1337  0.9  0.04  3.1   11  5    0.9

        hellHole startTime size timeFactor orbitX orbitY spinSpeed rotationSpeed =
            jump startTime $ fastForward timeFactor $ circle rotationSpeed $ orbit orbitX orbitY $ spin spinSpeed $ liftA (scale size size) displacementBall

        displacementBall :: Animation
        displacementBall t x y =
            distance 0 x 0 y >- \d ->
            gaussianOne 0.3 d >- \intensity ->
            rgba (intensity * x / d) (intensity * y / d) 0 1



--------------------------------
-- Some funky images
--------------------------------

redImage :: Image
redImage x y = rgba 1 0 0 1

greenImage :: Image
greenImage x y = rgba 0 1 0 1

blueImage :: Image
blueImage x y = rgba 0 0 1 1

cross :: Image
cross x y = rgba x y 0 1

rainbow :: Image
rainbow x _ = hsva (mod' x 1) 0.6 0.5 1

wave :: Image
wave x y =
    1 - abs (y - sin (x)) >- \intensity ->
    rgba intensity intensity intensity 1

gaussBall :: Image
gaussBall x y =
    distance 0 x 0 y >- \d ->
    gaussianOne 0.3 d >- \intensity ->
    rgba intensity intensity intensity 1

sharpBall :: Image
sharpBall x y =
    (distance 0 x 0 y) >- \d ->
    if' (d .<. 0.5) 1 0 >- \intensity ->
    rgba intensity intensity intensity 1

chess :: Image
chess x y =
    floor' (x `mod'` 2) >- \intensityX ->
    floor' (y `mod'` 2) >- \intensityY ->
    abs (intensityY - intensityX) >- \intensity ->
    rgba intensity intensity intensity 1


--------------------------------
-- Image blendings
--------------------------------

blender :: (R -> R -> R) -> Image -> Image -> Image
blender binOp f g x y =
    f x y >- \c1 ->
    g x y >- \c2 ->
    rgba (binOp (red c1) (red c2)) (binOp (green c1) (green c2)) (binOp (blue c1) (blue c2)) (alpha c1)

multiplyImage :: Image -> Image -> Image
multiplyImage = blender (*)

screenImage :: Image -> Image -> Image
screenImage = blender (\a b -> 1 - (1 - a) * (1 - b))

additionImage :: Image -> Image -> Image
additionImage = blender (\a b -> min' 1 (a + b))

subtractImage :: Image -> Image -> Image
subtractImage = blender (\a b -> max' 0 (a - b))


--------------------------------
-- Image transformations
--------------------------------

translate :: R -> R -> Image -> Image
translate dx dy image x y =
    image (dx + x) (dy + y)
    
scale :: R -> R -> Image -> Image
scale scaleX scaleY image x y =
    image (x / scaleX) (y / scaleY)

rotate :: R -> Image -> Image
rotate angle image x y =
    image (x * cos angle - y * sin angle) (x * sin angle + y * cos angle)



--------------------------------
-- Animation transformations
--------------------------------

jump :: Time -> Animation -> Animation
jump dt image t x y = image (t + dt) x y

scroll :: R -> R -> Animation -> Animation
scroll speedX speedY animation t =
    translate (speedX * t) (speedY * t) (animation t)

circle :: R -> Animation -> Animation
circle speed = orbit speed speed

orbit :: R -> R -> Animation -> Animation
orbit speedX speedY animation t = translate (cos (speedX * t)) (sin (speedY * t)) (animation t)

spin :: R -> Animation -> Animation
spin speed animation t = rotate (speed * t) (animation t)

fastForward :: R -> Animation -> Animation
fastForward speed animation t x y = animation (t * speed) x y

bendSpaceTime :: Animation -> Animation -> Animation
bendSpaceTime f target t x y =
    f t x y >- \spaceTimeColor ->
    red spaceTimeColor >- \dx ->
    green spaceTimeColor >- \dy ->
    blue spaceTimeColor >- \dt ->
    alpha spaceTimeColor >- \a ->
    target (t + dt * a) (x + dx * a) (y + dy * a)


--------------------------------
-- Animation blendings
--------------------------------

multiply :: Animation -> Animation -> Animation
multiply = liftA2 multiplyImage

screen :: Animation -> Animation -> Animation
screen = liftA2 screenImage

addition :: Animation -> Animation -> Animation
addition = liftA2 additionImage

subtract :: Animation -> Animation -> Animation
subtract = liftA2 subtractImage

--------------------------------
-- Utility stuff
--------------------------------
    
gaussian :: R -> R -> R
gaussian variance x =
    1 / sqrt(variance) * sqrt(2*pi)*exp(-(x*x)/(2*variance)**2)
    
gaussianOne :: R -> R -> R
gaussianOne variance x =
    gaussian variance 0 >- \maxValue ->
    gaussian variance x / maxValue

distance :: R -> R -> R -> R -> R
distance x1 x2 y1 y2 = sqrt ((x1 - x2)**2 + (y1 - y2)**2)

generateHtml :: Animation -> IO ()
generateHtml animation = do
    let f' = compile animation
    putStrLn f'
    writeFile "index.html" (before ++ f' ++ after)
    where
        before = "<html><head><title>Demo</title><style>body { margin: 0; }canvas { width: 100%; height: 100% }</style></head><body><script src=\"three.js\"></script><script id=\"fragmentShader\" type=\"x-shader/x-vertex\">\n//<![CDATA[\n"
        after = "//]]>\n</script><script src=\"program.js\"></script></body></html>\n"
