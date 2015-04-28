-- Needed for type family functions
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}

-- Needed for the Nat example
{-#LANGUAGE ScopedTypeVariables #-}

-- 7.6.3 for baseAddr
{-#LANGUAGE DataKinds #-}

module FunWithTypes() where

class Cons a b where
    type ResTy a b
    cons :: a-> [b] -> [ResTy a b]
    append :: [a] -> [b]

instance Cons Integer Double where
    type ResTy Integer Double = Double
    cons x ys = fromIntegral x : ys

data N = Z | Suc N

type One   = Suc Z
type Two   = Suc One
type Three = Suc Two
type Four  = Suc Three
type Six   = Suc (Suc Four)
type Eight = Suc (Suc Six)


type family GCD (d :: N) (m :: N) (n :: N) :: N
type instance GCD d Z Z             = d
type instance GCD d (Suc m) (Suc n) = GCD (Suc d) m n
type instance GCD Z (Suc m) Z       = Suc m
type instance GCD Z Z (Suc n)       = Suc n
type instance GCD (Suc d) (Suc m) Z = GCD (Suc Z) d m
type instance GCD (Suc d) Z (Suc n) = GCD (Suc Z) d n

class Nat (n::N) where
    toInt :: forall p. p n -> Int
instance Nat Z where
    toInt _ = 0
instance (Nat n) => Nat (Suc n) where
    toInt _ = 1 + toInt (undefined :: n)

-- Word Alignment in Bytes
newtype Alignment n = MkAlignment Int deriving Show
-- Byte address for a value
newtype Pointer   n = MkPointer Int deriving Show

-- Usage: baseAddr 4 :: Offset Three
baseAddr :: forall n. (Nat n) => Int -> Alignment n
baseAddr i = MkAlignment (i * toInt (undefined :: n))

verifyBoundary :: (GCD Z n Four ~ Four) => Pointer n
verifyBoundary = undefined
