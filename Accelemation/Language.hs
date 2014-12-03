{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Accelemation.Language where

import qualified Data.List as List
import Data.Maybe
import Control.Monad.State.Strict
import System.Environment (getProgName)

data Term'
    = Constant Double
    
    | Bind String Term' (Term' -> Term')
    | Variable Int

    | Field String Term'
    
    | If Term' Term' Term'

    | BuiltIn String
    
    | Call String Bool [Term']
    
    | LiftVec2 String Bool [Term']
    | LiftVec3 String Bool [Term']
    | LiftVec4 String Bool [Term']

newtype Term a = Term { unTerm :: Term' }

type R = Term Double
type Vec2 = Term (Double, Double)
type Vec3 = Term (Double, Double, Double)
type Vec4 = Term (Double, Double, Double, Double)

type Boolean = Term Bool

type Time = R
type Color = Vec4

type Image = R -> R -> Color
type Animation = Time -> Image


class HasX a where getX :: a -> R; withX :: R -> a -> a
class HasY a where getY :: a -> R; withY :: R -> a -> a
class HasZ a where getZ :: a -> R; withZ :: R -> a -> a
class HasW a where getW :: a -> R; withW :: R -> a -> a

instance HasX Vec2 where getX = Term . Field "x" . unTerm; withX r a = a >- \a' -> vec2 r (getY a')
instance HasX Vec3 where getX = Term . Field "x" . unTerm; withX r a = a >- \a' -> vec3 r (getY a') (getZ a')
instance HasX Vec4 where getX = Term . Field "x" . unTerm; withX r a = a >- \a' -> vec4 r (getY a') (getZ a') (getW a')
instance HasY Vec2 where getY = Term . Field "y" . unTerm; withY r a = a >- \a' -> vec2 (getX a') r
instance HasY Vec3 where getY = Term . Field "y" . unTerm; withY r a = a >- \a' -> vec3 (getX a') r (getZ a')
instance HasY Vec4 where getY = Term . Field "y" . unTerm; withY r a = a >- \a' -> vec4 (getX a') r (getZ a') (getW a')
instance HasZ Vec3 where getZ = Term . Field "z" . unTerm; withZ r a = a >- \a' -> vec3 (getX a') (getY a') r
instance HasZ Vec4 where getZ = Term . Field "z" . unTerm; withZ r a = a >- \a' -> vec4 (getX a') (getY a') r (getW a')
instance HasW Vec4 where getW = Term . Field "w" . unTerm; withW r a = a >- \a' -> vec4 (getX a') (getY a') (getZ a') r 


vec2 :: R -> R -> Vec2
vec2 (Term x) (Term y) = Term (Call "vec2" False [x, y])

vec3 :: R -> R -> R -> Vec3
vec3 (Term x) (Term y) (Term z) = Term (Call "vec3" False [x, y, z])

vec4 :: R -> R -> R -> R -> Vec4
vec4 (Term x) (Term y) (Term z) (Term w) = Term (Call "vec4" False [x, y, z, w])


curry2 :: (Vec2 -> Term a) -> (R -> R -> Term a)
curry2 f x y = vec2 x y >- \a' -> f a'

curry3 :: (Vec3 -> Term a) -> (R -> R -> R -> Term a)
curry3 f x y z = vec3 x y z >- \a' -> f a'

curry4 :: (Vec4 -> Term a) -> (R -> R -> R -> R -> Term a)
curry4 f x y z w = vec4 x y z w >- \a' -> f a'

uncurry2 :: (R -> R -> Term a) -> (Vec2 -> Term a)
uncurry2 f a = a >- \a' -> f (getX a') (getY a')

uncurry3 :: (R -> R -> R -> Term a) -> (Vec3 -> Term a)
uncurry3 f a = a >- \a' -> f (getX a') (getY a') (getZ a')

uncurry4 :: (R -> R -> R -> R -> Term a) -> (Vec4 -> Term a)
uncurry4 f a = a >- \a' -> f (getX a') (getY a') (getZ a') (getW a')


class ComponentWise a where
    lift0 :: R -> a
    lift1 :: (R -> R) -> (a -> a)
    lift2 :: (R -> R -> R) -> (a -> a -> a)
    lift3 :: (R -> R -> R -> R) -> (a -> a -> a -> a)

instance ComponentWise Vec2 where
    lift0 k = k >- \k' -> vec2 k' k'
    lift1 f a = a >- \a' -> vec2 (f (getX a')) (f (getY a'))
    lift2 f a b = flip fromMaybe (vectorOperator2' LiftVec2 f a b) $ a >- \a' -> b >- \b' -> vec2 (f (getX a') (getX b')) (f (getY a') (getY b'))
    lift3 f a b c = a >- \a' -> b >- \b' -> c >- \c' -> vec2 (f (getX a') (getX b') (getX c')) (f (getY a') (getY b') (getY c'))

instance ComponentWise Vec3 where
    lift0 k = k >- \k' -> vec3 k' k' k'
    lift1 f a = a >- \a' -> vec3 (f (getX a')) (f (getY a')) (f (getZ a'))
    lift2 f a b = flip fromMaybe (vectorOperator2' LiftVec3 f a b) $ a >- \a' -> b >- \b' -> vec3 (f (getX a') (getX b')) (f (getY a') (getY b')) (f (getZ a') (getZ b'))
    lift3 f a b c = a >- \a' -> b >- \b' -> c >- \c' -> vec3 (f (getX a') (getX b') (getX c')) (f (getY a') (getY b') (getY c')) (f (getZ a') (getZ b') (getZ c'))

instance ComponentWise Vec4 where
    lift0 k = k >- \k' -> vec4 k' k' k' k'
    lift1 f a = a >- \a' -> vec4 (f (getX a')) (f (getY a')) (f (getZ a')) (f (getW a'))
    lift2 f a b = flip fromMaybe (vectorOperator2' LiftVec4 f a b) $ a >- \a' -> b >- \b' -> vec4 (f (getX a') (getX b')) (f (getY a') (getY b')) (f (getZ a') (getZ b')) (f (getW a') (getW b'))
    lift3 f a b c = a >- \a' -> b >- \b' -> c >- \c' -> vec4 (f (getX a') (getX b') (getX c')) (f (getY a') (getY b') (getY c')) (f (getZ a') (getZ b') (getZ c')) (f (getW a') (getW b') (getW c'))


vectorOperator2' :: (String -> Bool -> [Term'] -> Term') -> (R -> R -> R) -> Term a -> Term a -> Maybe (Term a)
vectorOperator2' constructor f a b =
    let builtIn = ["+", "-", "*", "/", "mix"] in
    let Term f' = f (Term (BuiltIn "_1")) (Term (BuiltIn "_2")) in
    case f' of 
        Call function isOperator [BuiltIn "_1", BuiltIn "_2"] | elem function builtIn ->
            Just $ Term $ constructor function isOperator [unTerm a, unTerm b]
        _ -> Nothing


rgba :: R -> R -> R -> R -> Color
rgba = vec4

hsva :: R -> R -> R -> R -> Color
hsva (Term x) (Term y) (Term z) (Term w) = Term (Call "hsvaToRgba" False [Call "vec4" False [x, y, z, w]])

red, green, blue, alpha :: Color -> R
red = getX
green = getY
blue = getZ
alpha = getW


-- The mix function returns the linear blend of x and y, i.e. the product of x and (1 - a) plus the product of y and a.
mix :: R -> R -> R -> R
mix (Term x) (Term y) (Term a) = Term $ Call "mix" False [x, y, a]

-- = min(max(a, minVal), maxVal) 
clamp :: R -> R -> R -> R
clamp (Term x) (Term a) (Term b) = Term $ Call "clamp" False [x, a, b]

-- The cross product
cross :: Vec3 -> Vec3 -> Vec3
cross (Term a) (Term b) = Term $ Call "cross" False [a, b]

-- Please see: http://en.wikibooks.org/wiki/GLSL_Programming/Vector_and_Matrix_Operations
class VectorOperations a where
    dot :: Term a -> Term a -> R
    magnitude :: Term a -> R
    distance :: Term a -> Term a -> R
    normalize :: Term a -> Term a
    faceforward :: Term a -> Term a -> Term a -> Term a
    reflect :: Term a -> Term a -> Term a

    dot (Term a) (Term b) = Term $ Call "dot" False [a, b]
    magnitude (Term a) = Term $ Call "length" False [a]
    distance (Term a) (Term b) = Term $ Call "distance" False [a, b]
    normalize (Term a) = Term $ Call "normalize" False [a]
    faceforward (Term a) (Term b) (Term c) = Term $ Call "faceforward" False [a, b, c]
    reflect (Term a) (Term b) = Term $ Call "reflect" False [a, b]

instance VectorOperations (Double, Double) -- Vec2
instance VectorOperations (Double, Double, Double) -- Vec3
instance VectorOperations (Double, Double, Double, Double) -- Vec4
        

instance Num R where
    (Term a) + (Term b) = Term (Call "+" True [a, b])
    (Term a) - (Term b) = Term (Call "-" True [a, b])
    (Term a) * (Term b) = Term (Call "*" True [a, b])
    negate (Term a)     = Term (Call "-" True [a])
    abs (Term a)        = Term (Call "abs" False [a])
    signum (Term a)     = Term (Call "sign" False [a])
    fromInteger i       = Term (Constant (fromInteger i))

instance Fractional R where
    fromRational r      = Term (Constant (fromRational r))
    (Term a) / (Term b) = Term (Call "/" True [a, b])

instance Floating R where
    pi                  = Term (BuiltIn "pi")
    Term a ** Term b    = Term (Call "pow" False [a, b])
    sqrt (Term a)       = Term (Call "sqrt" False [a])
    exp (Term a)        = Term (Call "exp" False [a])
    log (Term a)        = Term (Call "log" False [a])
    sin (Term a)        = Term (Call "sin" False [a])
    tan (Term a)        = Term (Call "tan" False [a])
    cos (Term a)        = Term (Call "cos" False [a])
    asin (Term a)       = Term (Call "asin" False [a])
    atan (Term a)       = Term (Call "atan" False [a])
    acos (Term a)       = Term (Call "acos" False [a])
    sinh (Term a)       = Term (Call "sinh" False [a])
    tanh (Term a)       = Term (Call "tanh" False [a])
    cosh (Term a)       = Term (Call "cosh" False [a])
    asinh (Term a)      = Term (Call "asinh" False [a])
    atanh (Term a)      = Term (Call "atanh" False [a])
    acosh (Term a)      = Term (Call "acosh" False [a])

max', min', mod' :: R -> R -> R
max' (Term a) (Term b) = Term (Call "max" False [a, b])
min' (Term a) (Term b) = Term (Call "min" False [a, b])
mod' (Term a) (Term b) = Term (Call "mod" False [a, b])

round', floor', ceil' :: R -> R
round' (Term a) = Term (Call "round" False [a])
floor' (Term a) = Term (Call "floor" False [a])
ceil' (Term a) = Term (Call "ceil" False [a])

if' :: Boolean -> Term a -> Term a -> Term a
if' (Term a) (Term b) (Term c) = Term (If a b c)

(.==.) :: R -> R -> Boolean
Term a .==. Term b = Term (Call "==" True [a, b])

(./=.) :: R -> R -> Boolean
Term a ./=. Term b = Term (Call "!=" True [a, b])

(.<.) :: R -> R -> Boolean
Term a .<. Term b = Term (Call "<" True [a, b])

(.>.) :: R -> R -> Boolean
Term a .>. Term b = Term (Call ">" True [a, b])

(.<=.) :: R -> R -> Boolean
Term a .<=. Term b = Term (Call "<=" True [a, b])

(.>=.) :: R -> R -> Boolean
Term a .>=. Term b = Term (Call ">=" True [a, b])

(.&&.) :: Boolean -> Boolean -> Boolean
Term a .&&. Term b = Term (Call "&&" True [a, b])

(.||.) :: Boolean -> Boolean -> Boolean
Term a .||. Term b = Term (Call "||" True [a, b])

class Bindable a            where variableType :: a -> String
instance Bindable R         where variableType _ = "float"
instance Bindable Boolean   where variableType _ = "bool"
instance Bindable Vec2      where variableType _ = "vec2"
instance Bindable Vec3      where variableType _ = "vec3"
instance Bindable Vec4      where variableType _ = "vec4"

infixr 0 >-
(>-) :: Bindable (Term a) => Term a -> (Term a -> Term b) -> Term b
a@(Term x) >- f = Term (Bind (variableType a) x (unTerm . f . Term))

compile :: Animation -> String
compile f = before ++ bindings ++ "    gl_FragColor = " ++ compiled ++ after
    where
        bindings        = concat (reverse vs)
        (compiled, vs)  = runState (compile' animation) []
        Term animation  = f t x y
        t               = Term (BuiltIn "t")
        x               = Term (BuiltIn "x")
        y               = Term (BuiltIn "y")
        before          = 
            "uniform float u_time;\n" ++
            "uniform float u_aspectRatio;\n" ++
            "uniform vec2  u_resolution;\n" ++
            "vec4 hsvaToRgba(vec4 c) {\n" ++
            "    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);\n" ++
            "    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);\n" ++
            "    vec3 r = c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);\n" ++
            "    return vec4(r.x, r.y, r.z, c.w);\n" ++
            "}\n" ++
            "void main() {\n" ++
            "    float pi = " ++ show pi ++ ";\n" ++
            "    float t = u_time;\n" ++
            "    float mystery = 1.23;\n" ++
            "    float x = (gl_FragCoord.x / u_resolution.x) * 2.0 * u_aspectRatio - u_aspectRatio * mystery;\n" ++
            "    float y = (gl_FragCoord.y / u_resolution.y) * 2.0 - 1.0 * mystery;\n"
        after           =
            ";\n}\n"

            
compile' :: Term' -> State [String] String
compile' (Constant r) = return $ show r
compile' (Field l r) = do
    r' <- compile' r
    return (r' ++ "." ++ l)
compile' (If a b c) = do
    a' <- compile' a
    b' <- compile' b
    c' <- compile' c
    return $ "(" ++ a' ++ " ? " ++ b' ++ " : " ++ c' ++ ")"
compile' (Call f o es) = do
    es' <- mapM compile' es
    return $ case (o, es') of
        (True, [e1']) -> "(" ++ f ++ e1' ++ ")"
        (True, [e1', e2']) -> "(" ++ e1' ++ " " ++ f ++ " " ++ e2' ++ ")"
        (_, _) -> f ++ "(" ++ List.intercalate ", " es' ++ ")"
compile' (BuiltIn x) = return x
compile' (LiftVec2 f o es) = compile' (Call f o es)
compile' (LiftVec3 f o es) = compile' (Call f o es)
compile' (LiftVec4 f o es) = compile' (Call f o es)
compile' (Bind t x f) = do
    x' <- compile' x
    vs <- get
    let i = length vs
    let v = "    " ++ t ++ " v" ++ show i ++ " = " ++ x' ++ ";\n"
    put (v:vs)
    f' <- compile' (f (Variable i))
    return f'
compile' (Variable i) = return $ "v" ++ show i

generateHtml :: Animation -> IO ()
generateHtml animation = do
    let f' = compile animation
    putStrLn f'
    name <- getProgName
    writeFile (name ++ ".html") (before ++ f' ++ after)
    where
        before = "<html><head><title>Demo</title><style>body { margin: 0; }canvas { width: 100%; height: 100% }</style></head><body><script src=\"js/three.js\"></script><script id=\"fragmentShader\" type=\"x-shader/x-vertex\">\n//<![CDATA[\n"
        after = "//]]>\n</script><script src=\"js/program.js\"></script></body></html>\n"

instance Show a => Show (Term a) where
    show (Term term') = show term'

instance Show Term' where
    show term = bindings ++ compiled
        where
            bindings        = concat (reverse vs)
            (compiled, vs)  = runState (showM term) []

            showM :: Term' -> State [String] String
            showM (Constant r) = return $ show r
            showM (Field l r) = do
                r' <- showM r
                return (r' ++ "." ++ l)
            showM (If a b c) = do
                a' <- showM a
                b' <- showM b
                c' <- showM c
                return $ "(" ++ a' ++ " ? " ++ b' ++ " : " ++ c' ++ ")"
            showM (BuiltIn x) = return x
            showM (Call f o es) = do
                es' <- mapM showM es
                return $ case (o, es') of
                    (True, [e1']) -> "(" ++ f ++ e1' ++ ")"
                    (True, [e1', e2']) -> "(" ++ e1' ++ " " ++ f ++ " " ++ e2' ++ ")"
                    (_, _) -> f ++ "(" ++ List.intercalate ", " es' ++ ")"
            showM (LiftVec2 f o es) = showM $ Call f o es
            showM (LiftVec3 f o es) = showM $ Call f o es
            showM (LiftVec4 f o es) = showM $ Call f o es
            showM (Bind t x f) = do
                x' <- showM x
                vs <- get
                let i = length vs
                let v = "    " ++ t ++ " v" ++ show i ++ " = " ++ x' ++ ";\n"
                put (v:vs)
                f' <- showM (f (Variable i))
                return f'
            showM (Variable i) = return $ "v" ++ show i
