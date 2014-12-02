{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Accelemation.Language where

import qualified Data.List as List
import Control.Monad.State.Strict

data Term'
    = Constant Double
    
    | Bind String Term' (Term' -> Term')
    | Variable Int

    | Field String Term'
    
    | If Term' Term' Term'

    | Call String [Term']
    | BuiltIn String
    | UnaryOperator String Term'
    | BinaryOperator String Term' Term'

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


class HasX a where getX :: a -> R
class HasY a where getY :: a -> R
class HasZ a where getZ :: a -> R
class HasW a where getW :: a -> R

instance HasX (Term (Double, Double))                   where getX = Term . Field "x" . unTerm
instance HasX (Term (Double, Double, Double))           where getX = Term . Field "x" . unTerm
instance HasX (Term (Double, Double, Double, Double))   where getX = Term . Field "x" . unTerm
instance HasY (Term (Double, Double))                   where getY = Term . Field "y" . unTerm
instance HasY (Term (Double, Double, Double))           where getY = Term . Field "y" . unTerm
instance HasY (Term (Double, Double, Double, Double))   where getY = Term . Field "y" . unTerm
instance HasZ (Term (Double, Double, Double))           where getZ = Term . Field "z" . unTerm
instance HasZ (Term (Double, Double, Double, Double))   where getZ = Term . Field "z" . unTerm
instance HasW (Term (Double, Double, Double, Double))   where getW = Term . Field "w" . unTerm


vec2 :: R -> R -> Vec2
vec2 (Term x) (Term y) = Term (Call "vec2" [x, y])

vec3 :: R -> R -> R -> Vec3
vec3 (Term x) (Term y) (Term z) = Term (Call "vec3" [x, y, z])

vec4 :: R -> R -> R -> R -> Vec4
vec4 (Term x) (Term y) (Term z) (Term w) = Term (Call "vec4" [x, y, z, w])

rgba :: R -> R -> R -> R -> Color
rgba = vec4

hsva :: R -> R -> R -> R -> Color
hsva (Term x) (Term y) (Term z) (Term w) = Term (Call "hsvaToRgba" [Call "vec4" [x, y, z, w]])

red, green, blue, alpha :: Color -> R
red = getX
green = getY
blue = getZ
alpha = getW


instance Num R where
    (Term a) + (Term b) = Term (BinaryOperator "+" a b)
    (Term a) - (Term b) = Term (BinaryOperator "-" a b)
    (Term a) * (Term b) = Term (BinaryOperator "*" a b)
    negate (Term a)     = Term (UnaryOperator "-" a)
    abs (Term a)        = Term (Call "abs" [a])
    signum (Term a)     = Term (Call "sign" [a])
    fromInteger i       = Term (Constant (fromInteger i))

instance Fractional R where
    fromRational r      = Term (Constant (fromRational r))
    (Term a) / (Term b) = Term (BinaryOperator "/" a b)

instance Floating R where
    pi                  = Term (BuiltIn "pi")
    Term a ** Term b    = Term (Call "pow" [a, b])
    sqrt (Term a)       = Term (Call "sqrt" [a])
    exp (Term a)        = Term (Call "exp" [a])
    log (Term a)        = Term (Call "log" [a])
    sin (Term a)        = Term (Call "sin" [a])
    tan (Term a)        = Term (Call "tan" [a])
    cos (Term a)        = Term (Call "cos" [a])
    asin (Term a)       = Term (Call "asin" [a])
    atan (Term a)       = Term (Call "atan" [a])
    acos (Term a)       = Term (Call "acos" [a])
    sinh (Term a)       = Term (Call "sinh" [a])
    tanh (Term a)       = Term (Call "tanh" [a])
    cosh (Term a)       = Term (Call "cosh" [a])
    asinh (Term a)      = Term (Call "asinh" [a])
    atanh (Term a)      = Term (Call "atanh" [a])
    acosh (Term a)      = Term (Call "acosh" [a])

max', min', mod' :: R -> R -> R
max' (Term a) (Term b) = Term (Call "max" [a, b])
min' (Term a) (Term b) = Term (Call "min" [a, b])
mod' (Term a) (Term b) = Term (Call "mod" [a, b])

round', floor', ceil' :: R -> R
round' (Term a) = Term (Call "round" [a])
floor' (Term a) = Term (Call "floor" [a])
ceil' (Term a) = Term (Call "ceil" [a])

if' :: Boolean -> Term a -> Term a -> Term a
if' (Term a) (Term b) (Term c) = Term (If a b c)

(.==.) :: R -> R -> Boolean
Term a .==. Term b = Term (BinaryOperator "==" a b)

(./=.) :: R -> R -> Boolean
Term a ./=. Term b = Term (BinaryOperator "!=" a b)

(.<.) :: R -> R -> Boolean
Term a .<. Term b = Term (BinaryOperator "<" a b)

(.>.) :: R -> R -> Boolean
Term a .>. Term b = Term (BinaryOperator ">" a b)

(.<=.) :: R -> R -> Boolean
Term a .<=. Term b = Term (BinaryOperator "<=" a b)

(.>=.) :: R -> R -> Boolean
Term a .>=. Term b = Term (BinaryOperator ">=" a b)

(.&&.) :: Boolean -> Boolean -> Boolean
Term a .&&. Term b = Term (BinaryOperator "&&" a b)

(.||.) :: Boolean -> Boolean -> Boolean
Term a .||. Term b = Term (BinaryOperator "||" a b)

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
compile' (Call f es) = do
    es' <- mapM compile' es
    return $ f ++ "(" ++ List.intercalate ", " es' ++ ")"
compile' (BuiltIn x) = return x
compile' (UnaryOperator o a) = do
    a' <- compile' a
    return $ "(" ++ o ++ a' ++ ")"
compile' (BinaryOperator o a b) = do
    a' <- compile' a
    b' <- compile' b
    return $ "(" ++ a' ++ " " ++ o ++ " " ++ b' ++ ")"
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
    writeFile "index.html" (before ++ f' ++ after)
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
            showM (Call f es) = do
                es' <- mapM showM es
                return $ f ++ "(" ++ List.intercalate ", " es' ++ ")"
            showM (BuiltIn x) = return x
            showM (UnaryOperator o a) = do
                a' <- showM a
                return $ "(" ++ o ++ a' ++ ")"
            showM (BinaryOperator o a b) = do
                a' <- showM a
                b' <- showM b
                return $ "(" ++ a' ++ " " ++ o ++ " " ++ b' ++ ")"
            showM (Bind t x f) = do
                x' <- showM x
                vs <- get
                let i = length vs
                let v = "    " ++ t ++ " v" ++ show i ++ " = " ++ x' ++ ";\n"
                put (v:vs)
                f' <- showM (f (Variable i))
                return f'
            showM (Variable i) = return $ "v" ++ show i