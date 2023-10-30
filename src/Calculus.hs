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
  negate a
        | Val 0 <- a = 0
        | otherwise = Neg a
  (-) a b = a + negate b
  (+) a b
        | Val 0 <- a = b
        | Val 0 <- b = a
        | otherwise = Add a b
  (*) a b
        | Val 1 <- a = b
        | Val 1 <- b = a
        | Val 0 <- a = 0
        | Val 0 <- b = 0
        | otherwise = Mul a b

instance Fractional Expr where
  fromRational = Val . fromRational
  (/) a b
        | Val 0 <- a = 0
        | Val 0 <- b = undefined
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
        Add x y -> ex + ey
        Neg x -> negate ex
        Mul x y -> ex * ey
        Div x y -> ex / ey
        Sin x -> sin ex
        Cos x -> cos ex
        Log x -> log ex
        where
                ex = eval x env
                ey = eval y env

{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}
showExpr :: Expr -> String
showExpr expr = case expr of
        Val x -> show x
        Id x -> x
        Add x (Neg y) -> "(" ++ sx ++ " - " ++ sy ++ ")"
        Add x y -> "(" ++ sx ++ " + " ++ sy ++ ")"
        Neg x -> "-(" ++ sx ++ ")"
        Mul x y -> "(" ++ sx ++ " * " ++ sy ++ ")"
        Div x y -> "(" ++ sx ++ " / " ++ sy ++ ")"
        Sin x -> "sin (" ++ sx ++ ")"
        Cos x -> "cos (" ++ sx ++ ")"
        Log x -> "log (" ++ sx ++ ")"
        where
                sx = showExpr x
                sy = showExpr y

{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: Expr -> String -> Expr
diff (Id x) st
        | x == st = 1
        | otherwise = 0
diff expr st = case expr of
        Val x -> 0
        Add x y -> diffX + diffY
        Mul x y -> (x * diffY) + (diffX * y)
        Div x y -> ((diffX * y) - (x * diffY)) / (y * y)
        Sin x -> cos x * diffX
        Cos x -> negate (sin x * diffX)
        Log x -> diffX / x
        Neg x -> negate diffX
        where
                diffX = diff x st
                diffY = diff y st

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