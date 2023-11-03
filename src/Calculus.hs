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
-- deriving instance Show Expr
instance Show Expr where 
        show = showExpr

instance Num Expr where
  fromInteger = Val . fromIntegral
  negate (Neg a) = a
  negate a
        | Val 0 <- a = 0
        | otherwise = Neg a
  (-) a b 
        | Val 0 <- a = negate b
        | Val 0 <- b = a
        | otherwise = a + negate b
  (+) a b
        | Val 0 <- a = b
        | Val 0 <- b = a
        | Id c <- a
        , Id c <- b = 2 * Id c
        | Val c <- a
        , Val d <- b = Val (c + d)
        | otherwise = Add a b
  (*) a b
        | Val 1 <- a = b
        | Val 1 <- b = a
        | Val 0 <- a = 0
        | Val 0 <- b = 0
        | Val c <- a
        , Mul (Val d) (Id e) <- b = Val (c * d) * Id e
        | Val c <- b
        , Mul (Val d) (Id e) <- a = Val (c * d) * Id e
        | otherwise = Mul a b

instance Fractional Expr where
  fromRational = Val . fromRational
  (/) a b
        | Val 0 <- a = 0
        | Val 1 <- b = a
        | otherwise = Div a b

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
eval expr env = case expr of
        Val x -> x
        Id x -> lookUp x env
        Add x y -> eval x env + eval y env
        Neg x -> negate (eval x env)
        Mul x y -> eval x env * eval y env
        Div x (Val 0) -> undefined
        Div x y -> eval x env / eval y env
        Sin x -> sin (eval x env)
        Cos x -> cos (eval x env)
        Log x -> log (eval x env)

{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}
showExpr :: Expr -> String
showExpr expr = case expr of
        Val x -> show x
        Id x -> x
        Add x (Neg y) -> "(" ++ showExpr x ++ " - " ++ showExpr y ++ ")"
        Add x y -> "(" ++ showExpr x ++ " + " ++ showExpr y ++ ")"
        Neg x -> "-(" ++ showExpr x ++ ")"
        Mul x y -> "(" ++ showExpr x ++ " * " ++ showExpr y ++ ")"
        Div x y -> "(" ++ showExpr x ++ " / " ++ showExpr y ++ ")"
        Sin x -> "sin (" ++ showExpr x ++ ")"
        Cos x -> "cos (" ++ showExpr x ++ ")"
        Log x -> "log (" ++ showExpr x ++ ")"

{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: Expr -> String -> Expr
diff expr st = case expr of
        Id x -> if x == st then 1 else 0
        Val x -> 0
        Add x y -> diff x st + diff y st
        Mul x y -> (x * diff y st) + (diff x st * y)
        Div x y -> ((diff x st * y) - (x * diff y st)) / (y * y)
        Sin x -> cos x * diff x st
        Cos x -> negate (sin x * diff x st)
        Log x -> diff x st / x
        Neg x -> negate (diff x st)

{-|
Computes the approximation of an expression `f` by expanding the Maclaurin
series on `f` and taking its summation.
-}
maclaurin :: Expr   -- ^ expression to approximate (with `x` free)
          -> Double -- ^ value to give to `x`
          -> Int    -- ^ number of terms to expand
          -> Double -- ^ the approximate result
maclaurin expr x t = helper expr 0 1
        where
                helper :: Expr -> Int -> Int -> Double
                helper expr i fac
                        | i < t = (f_0 * (x ^^ i) / fromIntegral fac) + f'_0
                        | otherwise = 0
                        where
                                f_0 = eval expr [("x", 0)]
                                f'_0 = helper (diff expr "x") (i + 1) (fac * (i + 1))