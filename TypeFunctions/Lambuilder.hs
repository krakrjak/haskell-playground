-- From page 25 of Fun With Type Functions
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE GADTs #-}

module LamBuilder() where

data LamExpr a where
	Lit  :: Int -> LamExpr Int
	Add :: LamExpr Int -> LamExpr Int -> LamExpr Int
	App :: LamExpr (a -> b) -> LamExpr a -> LamExpr b

eval :: LamExpr a -> a
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (App e1 e2) = eval e1 (eval e2)
