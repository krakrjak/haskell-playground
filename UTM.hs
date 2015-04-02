-- A Universal Turing Machine Implementation in Haskell
-- Author: Zac Slade
-- Date: 2015-03-11
-- Inspiration for the list zipper used here from Boyd Stephen Smith Jr.
module Main(main) where

data TMState a = TMState a | H
data HeadMove = L | R
data TapeZ a = Repeat a | Zip (TapeZ a) a (TapeZ a)

current :: TapeZ a -> a
current z@(Repeat a)  = a
current z@(Zip _ c _) = c

write :: a -> TapeZ a -> TapeZ a
write x z@(Repeat _) = Zip z x z
write x z@(Zip l c r) = Zip l c r

moveLeft :: TapeZ a -> TapeZ a
moveLeft z@(Repeat _) = z
moveLeft z@(Zip l c r) = let n = Zip (moveLeft l) (current l) (Zip n c r) in n

moveRight :: TapeZ a -> TapeZ a
moveRight z@(Repeat _) = z
moveRight z@(Zip l c r) = let n = Zip (Zip l c n) (current r) (moveRight r) in n

extractActive :: TapeZ a -> [a]
extractActive = extractRight . resetLeft
    where
	resetLeft z@(Repeat _)           = z
	resetLeft z@(Zip (Repeat _) _ _) = z
	resetLeft z                      = resetLeft $ moveLeft z
	extractRight z@(Repeat _)  = []
	extractRight z@(Zip _ c r) = c : extractRight r


type Step a b = TMState b -> a -> (a, TMState b, HeadMove)

univTM :: b -> TapeZ a -> Step a b -> TapeZ a
univTM st z fun = snd $ simulation initSt z fun
    where
	initSt = TMState st

simulation :: TMState b -> TapeZ a -> Step a b -> (TMState b, TapeZ a)
simulation H z fun = (H, z)
simulation s z fun = simulation newSt newZ fun
    where
    (newSym, newSt, move) = fun s (current z)
    newZ = updateHead newSym move z

updateHead :: a -> HeadMove -> TapeZ a -> TapeZ a
updateHead a L z = moveLeft $ write a z
updateHead a R z = moveRight $ write a z

tape1 = ['1','0','1','0','0']
tape2 = ['1','0','1','0','1']
tape3 = ['1','1','1','0','0']

mkTape:: [a] -> TapeZ a
mkTape (h:t) = foldl (\zipp val -> write val (moveRight zipp)) initTape t
    where
    initTape = Repeat h

main = print . show $ extractActive theTM
    where
    theTM = univTM "start" (mkTape tape1) transFunc

-- TODO: Write some transition functions and test the universality
transFunc :: Step a b
transFunc st symb = undefined
