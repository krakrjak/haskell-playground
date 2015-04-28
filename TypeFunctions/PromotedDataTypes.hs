{-#LANGUAGE GADTs #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE StandaloneDeriving #-}

module PromotedDataTypes() where

-- FC-pro version - Value and type constructors
data Nat = Zero | Suc Nat

-- Indexed type family (type functions)
type family Plus (a :: Nat) (b :: Nat) :: Nat
type instance Plus Zero b    = b
type instance Plus (Suc a) b = Suc (Plus a b)

-- are promoted to type and kind constructors respectively.
data Vec :: * -> Nat -> * where
    Nil  :: Vec a Zero
    Cons :: a -> Vec a n -> Vec a (Suc n)
deriving instance Show a => Show (Vec a n)

concatVec :: Vec a (n :: Nat) -> Vec a (m::Nat) -> Vec a (Plus n m)
concatVec Nil         v2 = v2
concatVec (Cons a v1) v2 = Cons a $ concatVec v1 v2

lengthVec :: Vec a (n :: Nat) -> Int
lengthVec Nil = 0
lengthVec (Cons a v1) = (+) 1 (lengthVec v1)
