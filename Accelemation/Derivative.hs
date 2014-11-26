module Derivative (derivative) where

import Accelemation.Language

derivative :: (R -> R) -> R -- TODO return a function (R -> R)
derivative f =
    let term = f (Term (Variable 0))
    in derivative' term

derivative' :: Term Double -> Term Double
derivative' (Term t) | isConstant t                = 0
--derivative' (Term (Bind String Term' (Term' -> Term')))
derivative' (Term (Variable 0))             = 1
--derivative' (Term (Field String Term'))     =
derivative' (Term (If a b c))               = if' (Term a) (derivative' (Term b)) (derivative' (Term c))

derivative' (Term (BinaryOperator "+" a b)) = (derivative' (Term a)) + (derivative' (Term b))
derivative' (Term (BinaryOperator "-" a b)) = (derivative' (Term a)) - (derivative' (Term b))
derivative' (Term (BinaryOperator "*" a b)) = (derivative' (Term a)) * (Term b) + (Term a) * (derivative' (Term b))
derivative' (Term (BinaryOperator "/" a b)) = ((derivative' (Term a)) * (Term b) - (Term a) * (derivative' (Term b))) / (Term b)**2
derivative' (Term (UnaryOperator "-" a))    = -(derivative' (Term a))

--derivative' (Term (Call "abs" [a])) =
--derivative' (Term (Call "sign" [a])) =
derivative' (Term (Call "pow" [a, b]))      = (Term a)**(Term b) * (derivative' (Term a)) * ((Term a)/(Term b)) + (derivative' (Term b)) * log (Term a)
derivative' (Term (Call "sqrt" [a]))        = 0.5 * sqrt (Term a)
derivative' t@(Term (Call "exp" [a]))       = t
derivative' (Term (Call "log" [a]))         = (derivative' (Term a)) / (Term a)
derivative' (Term (Call "sin" [a]))         = -(cos (Term a))
derivative' (Term (Call "cos" [a]))         = -(sin (Term a))
derivative' (Term (Call "tan" [a]))         = derivative' (sin (Term a) / cos (Term a))
derivative' (Term (Call "max" [a, b]))      = if' ((Term a) .>. (Term b)) (derivative' (Term a)) (derivative' (Term b))
derivative' (Term (Call "min" [a, b]))      = if' ((Term a) .<. (Term b)) (derivative' (Term a)) (derivative' (Term b))
derivative' (Term (Call "mod" [a, b]))      = derivative' (Term a)


isConstant :: Term' -> Bool
isConstant (Variable 0) = False
isConstant (Constant _) = True
isConstant (If a b c) = isConstant a && isConstant b && isConstant c
isConstant (BinaryOperator _ a b) = isConstant a && isConstant b
isConstant (UnaryOperator _ a) = isConstant a
isConstant (BuiltIn _) = True
isConstant (Call _ es) = all isConstant es
isConstant t = error $ "isConstant is unexhaustive for: " ++ show t