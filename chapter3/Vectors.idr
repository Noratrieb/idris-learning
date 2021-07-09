module Vectors

import Data.Vect

fourInts : Vect 4 Int
fourInts = [0, 1, 2, 3]

sixInts : Vect 6 Int
sixInts = [1, 2, 3, 4, 5, 6]

tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts

nInts : (n : Nat) -> Vect n Int
nInts Z = []
nInts (S k) = 1 :: nInts k

-- Word length


allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

insert : Ord elem => (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

-- insertion sort
insSort :Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted


-- Exercises
my_length : List a -> Nat
my_length [] = 0
my_length (_ :: xs) = S (my_length xs)

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = (my_reverse xs) ++ [x]


my_map : (a -> b) -> List a -> List b
my_map _ [] = []
my_map f (x :: xs) = (f x) :: my_map f xs

my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map _ [] = []
my_vect_map f (x :: xs) = (f x) :: my_vect_map f xs


lengthLong : Vect n elem -> Nat
lengthLong [] = 0
lengthLong (x :: xs) = S (lengthLong xs)


lengthLong2 : Vect n elem -> Nat
lengthLong2 {n} xs = n
