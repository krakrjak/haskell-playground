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
data KVec :: * -> Nat -> * where
    KNil  :: KVec a Zero
    KCons :: a -> KVec a n -> KVec a (Suc n)
deriving instance Show a => Show (KVec a n)

concatKVec :: KVec a (n :: Nat) -> KVec a (m::Nat) -> KVec a (Plus n m)
concatKVec KNil         v2 = v2
concatKVec (KCons a v1) v2 = KCons a $ concatKVec v1 v2

lengthKVec :: KVec a (n :: Nat) -> Int
lengthKVec KNil = 0
lengthKVec (KCons a v1) = (+) 1 (lengthKVec v1)
