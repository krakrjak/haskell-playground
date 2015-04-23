-- From page 25 of Fun With Type Functions
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE GADTs #-}

module LamBuilder() where

data LamExpr a where
	Lit  :: Int -> LamExpr Int
	Add  :: LamExpr Int -> LamExpr Int -> LamExpr Int
	Mult :: LamExpr Int -> LamExpr Int -> LamExpr Int
	App  :: LamExpr (a -> b) -> LamExpr a -> LamExpr b
	Comp :: LamExpr (b -> c) -> LamExpr (a -> b) -> LamExpr a -> LamExpr c

eval :: LamExpr a -> a
eval (Lit i) = i
eval (Add  e1 e2)   = eval e1 + eval e2
eval (Mult e1 e2)   = eval e1 * eval e2
eval (App  e1 e2)   = eval e1 $ eval e2
eval (Comp e1 e2 a) = eval e1 . eval e2 $ eval a
