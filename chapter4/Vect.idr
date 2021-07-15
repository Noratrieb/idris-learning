module Vector

import Data.Fin

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys


length : Vect n elem -> Nat
length {n} xs = n

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] [] = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

index : Fin n -> Vect n elem -> elem
index FZ (x :: xs) = x
index (FS k) (x :: xs) = index k xs

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
                     Nothing => Nothing
                     (Just idx) => Just (index idx xs)


vectTake : (m : Fin n) -> Vect n elem -> Vect (finToNat m) elem
vectTake FZ xs = []
vectTake (FS k) (x :: xs) = x :: vectTake k xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                (Just idx) => Just ((index idx xs) + (index idx ys))
