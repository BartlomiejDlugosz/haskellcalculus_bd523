{-# LANGUAGE StandaloneDeriving #-}
module Calculus (lookUp, eval, showExpr, diff, maclaurin) where

import Vars
import Expr

import Data.Maybe

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

-- Comment this out if you want to implement your own instance in terms
-- of `showExpr`
deriving instance Show Expr

instance Num Expr where
  fromInteger = undefined
  negate      = Neg
  (+)         = Add
  (*)         = Mul

instance Fractional Expr where
  fromRational = undefined
  (/)          = Div

instance Floating Expr where
  sin = Sin
  cos = Cos
  log = Log

---------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x = fromJust . lookup x

{-|
Evaluates a given expression, evaluating any variables to their value within
the provided environment.
-}
eval :: Expr -> Env -> Double
eval expr env
        | Val x <- expr = x
        | Id x <- expr = lookUp x env
        | Add x y <- expr = eval x env + eval y env
        | Neg x <- expr = negate (eval x env)
        | Mul x y <- expr = eval x env * eval y env
        | Div x y <- expr = eval x env / eval y env
        | Sin x <- expr = sin (eval x env)
        | Cos x <- expr = cos (eval x env)
        | Log x <- expr = log (eval x env)

{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}
showExpr :: Expr -> String
showExpr = undefined

{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: Expr -> String -> Expr
diff (Id x) st
        | x == st = Val 1
        | otherwise = Val 0
diff expr st 
        | Val x <- expr = Val 0
        | Add x y <- expr = diff x st + diff y st
        | Mul x y <- expr = (x * diff y st) + (diff x st * y)
        | Div x y <- expr = ((diff x st * y) - (x * diff y st)) / (y * y)
        | Sin x <- expr = cos x * diff x st
        | Cos x <- expr = negate (Sin x * diff x st)
        | Log x <- expr = diff x st / x
        | Neg x <- expr = negate (diff x st)

{-|
Computes the approximation of an expression `f` by expanding the Maclaurin
series on `f` and taking its summation.
-}
maclaurin :: Expr   -- ^ expression to approximate (with `x` free)
          -> Double -- ^ value to give to `x`
          -> Int    -- ^ number of terms to expand
          -> Double -- ^ the approximate result
maclaurin expr x 0 = eval expr [("x", x)]
maclaurin expr x t = helper expr 0
        where
                -- -> Double since the helper function requires it in terms of double
                factorial :: Int -> Double
                factorial n = fromIntegral (product [1..n])

                toPower :: Double -> Int -> Double
                toPower _ 0 = 1
                toPower x i = x * toPower x (i - 1)

                helper :: Expr -> Int -> Double
                helper expr i
                        | i < t = eval (Val (eval ((Val (eval expr [("x", 0)]) * Val (toPower x i)) / Val (factorial i)) [("x", x)]) + Val (helper (diff expr "x") (i + 1))) [("x", x)]
                        | otherwise = 0