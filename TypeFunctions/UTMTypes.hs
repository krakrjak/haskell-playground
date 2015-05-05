{-#LANGUAGE DataKinds #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE PolyKinds #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleContexts #-}

module UTMTypes() where

--GADT Natural Numbers, promoted as Types and Kinds
data Nat where
    Zero :: Nat
    Suc  :: Nat -> Nat

type Step a = Either (Char, TMState a) (Char, TMState a, HeadMove)

--Turing Machine States as types and kinds
data TMState a where
    Halt   :: TMState a
    NState :: a -> TMState a
    deriving (Show)

--Tape movements as types and kinds
data HeadMove where
    MoveLeft  :: HeadMove
    MoveRight :: HeadMove
    deriving (Show)

data VZip a = Zip [a] a [a]
    deriving (Show)

class Tape t where
    -- Proxy type to extract wrapped data type
    type Elem t
    -- Basic functions of the tape
    empty  :: t
    write  :: Elem t -> t -> t
    moveR  :: t -> t
    moveL  :: t -> t
    curr   :: t -> Elem t

    -- Extracting contents from the Tape
    -- resetL and extractR are helper functions
    resetL        :: t -> t
    extractR      :: t -> [Elem t]

    -- extractActive is the helper method to extract
    -- the Tape archive from the zipper into a list
    extractActive :: t -> [Elem t]

instance (a ~ Char) => Tape (VZip a) where
    type Elem (VZip a) = a

    -- Dirty hack that depends on the Sym type being Char
    empty = Zip [] '\0' []

    -- Write a new value to the tape
    write v (Zip ls c rs) = Zip ls v rs

    -- Move the tape head left
    moveL z@(Zip []   c rs) = Zip [] '\0' (c:rs)
    moveL (Zip ls@(h:t) c rs) = Zip (init ls) (last ls) (c:rs)

    -- Move the tape head right
    moveR (Zip ls c [])     = Zip (c:ls) '\0' []
    moveR (Zip ls c (r:rs)) = Zip (c:ls) r rs

    -- Extract the current element under focus in the list
    curr  (Zip _ c _)   = c

    -- Utilities to retrieve the contents of Tape
    resetL z@(Zip [] c rs)      = z
    resetL z@(Zip (l:ls) c rs)  = resetL $ moveL z
    extractR z@(Zip l c [])     = []
    extractR z@(Zip _ c (r:rs)) = c : extractR (Zip [] r rs)
    extractActive               = extractR . resetL

-- Move or Write and Move variation on Turing Machines
simulate :: (Tape t, Elem t ~ Char) => (TMState a -> Char -> Step a) -> TMState a-> t -> (TMState a, t)
simulate f Halt t = (Halt, t)
simulate f q    t = case f q (curr t) of
    Left  (sym, state)            -> (state, write sym t)
    Right (sym, state, MoveLeft)  -> (state, moveL $ write sym t)
    Right (sym, state, MoveRight) -> (state, moveR $ write sym t)

deltaFun :: TMState String -> Char -> Step String
deltaFun st sym = case sym of
    '0' -> case st of
        NState "start"  -> Right ('0', NState "start0", MoveRight)
        NState "start0" -> Right ('0', NState "start0", MoveRight)
        NState "start1" -> Right ('1', NState "start1", MoveRight)
        NState "rewind" -> Right ('0', NState "rewind", MoveLeft)
    '1' -> case st of
        NState "start"  -> Right ('1', NState "start1", MoveRight)
        NState "start0" -> Right ('0', NState "start0", MoveRight)
        NState "start1" -> Right ('1', NState "start1", MoveRight)
        NState "rewind" -> Right ('1', NState "rewind", MoveLeft)
    '\0' -> case st of
        NState "start"  -> Left  ('\0', Halt)
        NState "start0" -> Right ('\0', NState "rewind", MoveLeft)
        NState "start1" -> Right ('\0', NState "rewind", MoveLeft)
        NState "rewind" -> Right ('\0', Halt, MoveRight)
    _ -> Left (sym, Halt)

mkTape :: (Tape t, t ~ VZip Char) => [Char] -> t
mkTape []    = empty
mkTape xs@(h:t) = resetL $ foldr (\v zip -> moveR $ write v zip) empty xs

tape1 :: String
tape1 = "10100"

utm :: (Tape t, t ~ VZip Char) => (TMState a -> Char -> Step a) -> TMState a -> t -> t
utm fun st z = case simulate fun st z of
    (Halt, t) -> t
    (newSt, t) -> snd $ simulate fun newSt t

main = print . show $ extractActive $ utm deltaFun (NState "start") (mkTape tape1)
