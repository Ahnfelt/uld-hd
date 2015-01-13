module Accelemation.Derivative (
    derivative,
    GrayscaleImage, Grayscale,
    fromGrayscaleImage, fromGrayscale,
    derivativeGrayscale,
) where

import Accelemation.Language
import Control.Applicative (liftA)

type GrayscaleImage = R -> R -> R

-- Grayscale animation
type Grayscale = Time -> GrayscaleImage

fromGrayscaleImage :: GrayscaleImage -> Image
fromGrayscaleImage f x y = (f x y) >- \i ->
    rgba i i i 1

fromGrayscale :: Grayscale -> Animation
fromGrayscale = liftA fromGrayscaleImage

derivativeGrayscale :: Grayscale -> Animation
derivativeGrayscale f t x y =
    let r = derivative (\a -> f t a y) x
        g = derivative (\a -> f t x a) y
    in rgba r g 0 1

--derivativeNormals :: Grayscale -> Animation
--derivativeNormals f t x y = 


derivative :: (R -> R) -> (R -> R)
derivative f =
    let term = f (Term (BuiltIn "_x"))
        Term d = derivative' term
    in \(Term x) -> Term ((substisute x) d)
    where
        substisute :: Term' -> Term' -> Term'
        substisute x t = case t of
            Constant r -> t
            Field l r -> Field l (substisute x r)
            If a b c -> If (substisute x a) (substisute x b) (substisute x c)
            Call f o es -> Call f o (map (substisute x) es)
            LiftVec2 f o es -> LiftVec2 f o (map (substisute x) es)
            LiftVec3 f o es -> LiftVec3 f o (map (substisute x) es)
            LiftVec4 f o es -> LiftVec4 f o (map (substisute x) es)
            BuiltIn "_x" -> x
            BuiltIn x -> t
            Bind name value body -> Bind name (substisute x value) body -- TODO
            Variable i -> t

derivative' :: Term Double -> Term Double
derivative' t@(Term term') = case term' of
    _ | isConstant term'    -> 0
    BuiltIn "_x"            -> 1
    If a b c                -> if' (Term a) (derivative' (Term b)) (derivative' (Term c))
--  Field String Term'      ->
    Bind t v f              -> derivative' (Term (f v))
        -- derivative' (Term v) >- \v' -- TODO
    Call f True [a, b]      -> derivativeBinary f a b
    Call "-" True [a]       -> -(derivative' (Term a))
    Call "abs" False [a]    -> derivative' (Term a) >- \a' -> if' ((Term a) .>. 0) a' (-a') -- TODO remove common subexpression
    Call "sign" False [a]   -> 0
    Call "pow" False [a, b] -> derivativeBinary "pow" a b
    Call "sqrt" False [a]   -> 0.5 * sqrt (Term a) * derivative' (Term a)
    Call "exp" False [a]    -> t * derivative' (Term a)
    Call "log" False [a]    -> ((derivative' (Term a)) / (Term a)) * derivative' (Term a)
    Call "sin" False [a]    -> -(cos (Term a)) * derivative' (Term a)
    Call "cos" False [a]    -> -(sin (Term a)) * derivative' (Term a)
    Call "tan" False [a]    -> (derivative' (sin (Term a) / cos (Term a))) * derivative' (Term a)
    Call "max" False [a, b] -> if' ((Term a) .>. (Term b)) (derivative' (Term a)) (derivative' (Term b))
    Call "min" False [a, b] -> if' ((Term a) .<. (Term b)) (derivative' (Term a)) (derivative' (Term b))
    Call "mod" False [a, b] -> derivative' (Term a)
    LiftVec2 f o es         -> derivative' (Term $ Call "vec2" False [Call f o (map (Field "x") es), Call f o (map (Field "y") es)])
    LiftVec3 f o es         -> derivative' (Term $ Call "vec3" False [Call f o (map (Field "x") es), Call f o (map (Field "y") es), Call f o (map (Field "z") es)])
    LiftVec4 f o es         -> derivative' (Term $ Call "vec4" False [Call f o (map (Field "x") es), Call f o (map (Field "y") es), Call f o (map (Field "z") es), Call f o (map (Field "w") es)])


derivativeBinary op a b = case (op, isConstant a, isConstant b) of
    ("+", True, _) -> g'
    ("+", _, True) -> f'
    ("+", False, False) -> f' + g'

    ("-", True, _) -> g'
    ("-", _, True) -> f'
    ("-", False, False) -> f' - g'

    ("*", True, _) -> f * g'
    ("*", _, True) -> f' * g
    ("*", False, False) -> f' * g + f * g'

    ("/", True, _) -> f * (g'/g**2)
    ("/", _, True) -> f'/g
    ("/", False, False) -> (f' * g - f * g')/g**2

--    ("pow", True, _) ->
    ("pow", _, True) -> (f ** g) * f' * (g/f)
    ("pow", _, _) -> (f ** g) * (f'*(g/f) + g'* log f)
    where
        f = Term a
        g = Term b
        f' = derivative' (Term a)
        g' = derivative' (Term b)

isConstant :: Term' -> Bool
isConstant (BuiltIn "_x") = False
isConstant (Constant _) = True
isConstant (If a b c) = isConstant a && isConstant b && isConstant c
isConstant (BuiltIn _) = True
isConstant (Call "sign" False [_]) = True
isConstant (Call _ _ es) = all isConstant es
isConstant (LiftVec2 _ _ es) = all isConstant es
isConstant (LiftVec3 _ _ es) = all isConstant es
isConstant (LiftVec4 _ _ es) = all isConstant es
isConstant (Bind x v f) = isConstant v && isConstant (f (Constant 0))
