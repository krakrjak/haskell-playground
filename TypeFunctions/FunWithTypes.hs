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

instance Cons Integer Double where
    type ResTy Integer Double = Double
    cons x ys = fromIntegral x : ys

data Zero
data Succ n

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three
type Six   = Succ (Succ Four)
type Eight = Succ (Succ Six)

class Nat n where
    toInt :: n -> Int

instance Nat Zero where
    toInt _ = 0

instance (Nat n) => Nat (Succ n) where
    toInt _ = 1 + toInt (undefined :: n)

type family GCD d m n
type instance GCD d Zero Zero            = d
type instance GCD d (Succ m) (Succ n)    = GCD (Succ d) m n
type instance GCD Zero (Succ m) Zero     = Succ m
type instance GCD Zero Zero (Succ n)     = Succ n
type instance GCD (Succ d) (Succ m) Zero = GCD (Succ Zero) d m
type instance GCD (Succ d) Zero (Succ n) = GCD (Succ Zero) d n

newtype Pointer n = MkPointer Int
	deriving Show
newtype Offset  n = MkOffset Int
	deriving Show

-- Usage: baseAddr 4 :: Offset Three
baseAddr :: forall n. (Nat n) => Int -> Offset n
baseAddr i = MkOffset (i * toInt (undefined :: n))
