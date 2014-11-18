module Main where
import Pixel

-- Transform

translate :: R -> R -> Image -> Image
translate dx dy image x y =
    image (dx + x) (dy + y)
    
scale :: R -> R -> Image -> Image
scale scaleX scaleY image x y =
    image (x / scaleX) (y / scaleY)

    
-- Blend
    
blender :: (R -> R -> R) -> Image -> Image -> Image
blender binOp f g x y =
    f x y >- \c1 ->
    g x y >- \c2 ->
    rgba (binOp (red c1) (red c2)) (binOp (green c1) (green c2)) (binOp (blue c1) (blue c2)) (binOp (alpha c1) (alpha c2))

multiply :: Image -> Image -> Image
multiply = blender (*)

screen :: Image -> Image -> Image
screen = blender (\a b -> 1 - (1 - a) * (1 - b))

addition :: Image -> Image -> Image
addition = blender (\a b -> min' 1 (a + b))

subtract :: Image -> Image -> Image
subtract = blender (\a b -> max' 0 (a - b))    


-- Images    

rainbow :: Image
rainbow x _ = hsva (mod' x 1) 0.5 0.5 1

wave :: Image
wave x y =
    1 - abs (y - sin (x)) >- \intensity ->
    rgba intensity intensity intensity 1
        
ball :: Image    
ball x y =
    max' 0 (1 - sqrt(x * x + y * y)) >- \intensity ->
    rgba intensity intensity intensity 1

gaussBall :: Image
gaussBall x y =
    0.3 >- \variance ->
    sqrt(x * x + y * y) >- \distance ->
    gaussian variance 0 >- \maxGaussian ->
    gaussian variance distance / maxGaussian >- \intensity ->
    rgba intensity intensity intensity 1
    

-- Animations

jump :: Time -> Animation -> Animation
jump dt image t x y = image (t + dt) x y

scroll :: R -> R -> Image -> Animation
scroll speedX speedY image t =
    translate (speedX * t) (speedY * t) image

circle :: R -> Image -> Animation
circle speed image t = translate (cos (speed * t)) (sin (speed * t)) image

fastForward :: R -> Animation -> Animation
fastForward speed animation t x y = animation (t * speed) x y

futureHole :: Animation -> Animation
futureHole animation t x y =
    cos(t) * 0.3 >- \centerX ->
    sin(t) * 0.3 >- \centerY ->
    0.3 >- \variance ->
    sqrt((centerX - x)**2 + (centerY - y)**2) >- \distance ->
    gaussian variance 0 >- \maxGaussian ->
    gaussian variance distance / maxGaussian >- \intensity ->
    animation (t + t * intensity * sin t) x y
    
    
-- Utility    
    
gaussian :: R -> R -> R
gaussian variance x =
    1 / sqrt(variance) * sqrt(2*pi)*exp(-(x*x)/(2*variance)**2)
    
-- Tests    
    
f1 :: Animation
f1 t x y = rgba 0 (abs (sin t)) 0 1

f2 :: Animation
f2 t =
    translate (t * 50) (t * (-10)) $
    scale (t * 100) (t * 100) ball

f3 :: Animation
f3 t x y = rainbow (x + t * 10) y

theGame :: Animation
theGame t =
    let
        colorbow = scroll 1 0 (multiply rainbow wave)
        spinBall = circle 1 gaussBall
        balls t = ((jump 0 spinBall) t `addition` (jump (2*pi/3) spinBall) t) `addition` (jump (4*pi/3) spinBall) t
        combined t = addition (colorbow t) (balls t)
        timeBent = futureHole (scroll 1 0 rainbow)
    in timeBent t
    
-- Write the shader

main = do
    let f' = compile (\t -> theGame t)
    putStrLn f'
    writeFile "index.html" (before ++ f' ++ after)
    where
        before = "<html><head><title>Demo</title><style>body { margin: 0; }canvas { width: 100%; height: 100% }</style></head><body><script src=\"three.js\"></script><script id=\"fragmentShader\" type=\"x-shader/x-vertex\">\n//<![CDATA[\n"
        after = "//]]>\n</script><script src=\"program.js\"></script></body></html>\n"
