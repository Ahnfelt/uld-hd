module Accelemation.Derivative (
    derivative,
    GrayscaleImage, Grayscale, derivativeGrayscale
) where

import Accelemation.Language

type GrayscaleImage = R -> R -> R

-- Grayscale animation
type Grayscale = Time -> GrayscaleImage

derivativeGrayscale :: Grayscale -> Animation
derivativeGrayscale f t x y =
    let r = derivative (\a -> f t a y) x
        g = derivative (\a -> f t x a) y
    in rgba r g 0 1

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
            Call f es -> Call f (map (substisute x) es)
            BuiltIn "_x" -> x
            BuiltIn x -> t
            UnaryOperator o a -> UnaryOperator o (substisute x a)
            BinaryOperator o a b -> BinaryOperator o (substisute x a) (substisute x b)
            Bind name value body -> Bind name (substisute x value) body -- TODO
            Variable i -> t

derivative' :: Term Double -> Term Double
derivative' (Term t) | isConstant t                = 0
derivative' (Term (BuiltIn "_x"))             = 1
derivative' (Term (If a b c))               = if' (Term a) (derivative' (Term b)) (derivative' (Term c))
--derivative' (Term (Field String Term'))     =
--derivative' (Term (Bind String Term' (Term' -> Term')))

derivative' (Term (BinaryOperator op a b)) = derivativeBinary op a b
derivative' (Term (UnaryOperator "-" a))    = -(derivative' (Term a))

--derivative' (Term (Call "abs" [a])) =
--derivative' (Term (Call "sign" [a])) =
derivative' (Term (Call "pow" [a, b]))      = derivativeBinary "pow" a b
derivative' (Term (Call "sqrt" [a]))        = 0.5 * sqrt (Term a) * derivative' (Term a)
derivative' t@(Term (Call "exp" [a]))       = t * derivative' (Term a)
derivative' (Term (Call "log" [a]))         = ((derivative' (Term a)) / (Term a)) * derivative' (Term a)
derivative' (Term (Call "sin" [a]))         = -(cos (Term a)) * derivative' (Term a)
derivative' (Term (Call "cos" [a]))         = -(sin (Term a)) * derivative' (Term a)
derivative' (Term (Call "tan" [a]))         = (derivative' (sin (Term a) / cos (Term a))) * derivative' (Term a)
derivative' (Term (Call "max" [a, b]))      = if' ((Term a) .>. (Term b)) (derivative' (Term a)) (derivative' (Term b))
derivative' (Term (Call "min" [a, b]))      = if' ((Term a) .<. (Term b)) (derivative' (Term a)) (derivative' (Term b))
derivative' (Term (Call "mod" [a, b]))      = derivative' (Term a)


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
isConstant (BinaryOperator _ a b) = isConstant a && isConstant b
isConstant (UnaryOperator _ a) = isConstant a
isConstant (BuiltIn _) = True
isConstant (Call _ es) = all isConstant es
isConstant (Bind x v f) = isConstant v && isConstant (f (Constant 0))

