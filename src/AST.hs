module AST
  ( Expr(..)
  , evalExpr
  ) where

data Expr a where
  LitInt  :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add     :: Expr Int -> Expr Int -> Expr Int
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a
  Not     :: Expr Bool -> Expr Bool

evalExpr :: Expr a -> a
evalExpr (LitInt a) = a
evalExpr (LitBool a) = a
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (If cond t f) | evalExpr cond = evalExpr t
                       | otherwise     = evalExpr f
evalExpr (Not a) = not $ evalExpr a
