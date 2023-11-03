module Expr where

{-|
This is a datatyping representing arithmetic expressions.
-}
data Expr = Val Double | Id String
          | Add Expr Expr | Mul Expr Expr | Div Expr Expr
          | Neg Expr | Sin Expr | Cos Expr | Log Expr

{- Test cases from the spec. -}
e1, e2, e3, e4, e5, e6, e7 :: Expr

-- | 5*x
e1 =  Mul (Val 5.0) (Id "x")

-- | x*x + y - 7
e2 =  Add (Add (Mul (Id "x") (Id "x"))
               (Id "y"))
          (Neg (Val 7.0))

-- | x-y^2/(4*x*y-y^2)::Expr
e3 =  Add (Id "x")
          (Neg (Div (Mul (Id "y") (Id "y"))
                    (Add (Mul (Mul (Val 4.0) (Id "x"))
                              (Id "y"))
                         (Neg (Mul (Id "y") (Id "y"))))))

-- | -cos x :: Expr
e4 =  Neg (Cos (Id "x"))

-- | sin (1+log(2*x)) :: Expr
e5 =  Sin (Add (Val 1.0)
               (Log (Mul (Val 2.0) (Id "x"))))

-- | log(3*x^2+2) :: Expr
e6 =  Log (Add (Mul (Val 3.0)
                    (Mul (Id "x") (Id "x")))
               (Val 2.0))

-- (sin ((3.0 * x)) / (1.0 + (x * x)))
e7 = Div (Sin (Mul (Val 3.0) (Id "x")))
         (Add (Val 1.0) (Mul (Id "x") (Id "x")))