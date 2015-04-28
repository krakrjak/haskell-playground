{-#LANGUAGE GADTs #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE StandaloneDeriving #-}


module GadtPlayground() where

-- From page 25 of Fun With Type Functions
data LamExpr a where
	Lit  :: Int -> LamExpr Int
	Add  :: LamExpr Int -> LamExpr Int -> LamExpr Int
	Mult :: LamExpr Int -> LamExpr Int -> LamExpr Int
	App  :: LamExpr (a -> b) -> LamExpr a -> LamExpr b
	Comp :: LamExpr (b -> c) -> LamExpr (a -> b) -> LamExpr a -> LamExpr c

evalLam :: LamExpr a -> a
evalLam (Lit i) = i
evalLam (Add  e1 e2)   = evalLam e1 + evalLam e2
evalLam (Mult e1 e2)   = evalLam e1 * evalLam e2
evalLam (App  e1 e2)   = evalLam e1 $ evalLam e2
evalLam (Comp e1 e2 a) = evalLam e1 . evalLam e2 $ evalLam a

data Z   = Z   deriving Show
data S n = S n deriving Show

-- This indexed vector could be used to inhabit
-- nonsensical types like Vec Zero Int or Suc Bool.
-- FC-pro version from 2012 (pre-print 2011) fixes
-- this problem.
data Vec :: * -> * -> * where
    Nil  :: Vec a Z
    Cons :: a -> Vec a n -> Vec a (S n)
deriving instance Show a => Show (Vec a n)

