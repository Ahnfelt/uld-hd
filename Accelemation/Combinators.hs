module Accelemation.Combinators (
    translate, scale, scaleUniform, rotate,
    scroll, circle, orbit, spin,
    timeTravel, fastForward, bendSpaceTime, fromPolarCoordinates,
    multiply, screen, addition, subtract, top
) where

import Accelemation.Language
import Accelemation.Arithmetic
import Control.Applicative
import Prelude hiding (subtract)



translate :: R -> R -> Animation -> Animation
translate dx dy animation t x y =
    (animation t) (dx + x) (dy + y)

scale :: R -> R -> Animation -> Animation
scale scaleX scaleY animation t x y =
    (animation t) (x / scaleX) (y / scaleY)

scaleUniform :: R -> Animation -> Animation
scaleUniform factor = scale factor factor

rotate :: R -> Animation -> Animation
rotate angle animation t x y =
    (animation t) (x * cos angle - y * sin angle) (x * sin angle + y * cos angle)

scroll :: R -> R -> Animation -> Animation
scroll speedX speedY animation t =
    translate (speedX * t) (speedY * t) animation t

circle :: R -> Animation -> Animation
circle speed = orbit speed speed

orbit :: R -> R -> Animation -> Animation
orbit speedX speedY animation t =
    translate (cos (speedX * t)) (sin (speedY * t)) animation t

spin :: R -> Animation -> Animation
spin speed animation t = rotate (speed * t) animation t

timeTravel :: Time -> Animation -> Animation
timeTravel dt image t x y = image (t + dt) x y

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


fromPolarCoordinates :: Animation -> Animation
fromPolarCoordinates f t x y =
    sqrt (x**2 + y**2) >- \r ->
    atan2' x y >- \phi ->
    f t r phi

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

top :: Animation -> Animation -> Animation
top = liftA2 topImage

--------------------------------
-- Image blendings
--------------------------------

blender :: (R -> R -> R) -> Image -> Image -> Image
blender binOp f g x y =
    f x y >- \c1 ->
    g x y >- \c2 ->
    rgba (binOp (red c1) (red c2)) (binOp (green c1) (green c2)) (binOp (blue c1) (blue c2)) ((alpha c1 + alpha c2) / 2)

multiplyImage :: Image -> Image -> Image
multiplyImage = blender (*)

screenImage :: Image -> Image -> Image
screenImage = blender (\a b -> 1 - (1 - a) * (1 - b))

additionImage :: Image -> Image -> Image
additionImage = blender (\a b -> min' 1 (a + b))

subtractImage :: Image -> Image -> Image
subtractImage = blender (\a b -> max' 0 (a - b))

topImage :: Image -> Image -> Image
topImage f g x y =
    f x y >- \a ->
    g x y >- \b ->
    let combine component = component a * alpha a + component b * alpha b * (1 - min' 1 (alpha a))
    in rgba (combine red) (combine green) (combine blue) (alpha a + alpha b * (1 - min' 1 (alpha a)))

