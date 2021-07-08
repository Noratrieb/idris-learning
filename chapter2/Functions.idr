double : Num ty => ty -> ty
double x = x + x

add : Int -> Int -> Int
add x y = x + y

identity : ty -> ty
identity x = x

-- the
der : (ty : Type) -> ty -> ty
der ty x = x


||| Higher-Order-Functions

twice : (a -> a) -> a -> a
twice f x = f (f x)

Shape : Type

rotate : Shape -> Shape

quadruple : Num a => a -> a
quadruple = twice double

turn_around : Shape -> Shape
turn_around = twice rotate

apply_n : (a -> a) -> Nat -> a -> a

longer : String -> String -> Nat
longer word1 word2 =
    let len1 = length word1
        len2 = length word2 in
        max len1 len2


pythagoras : Double -> Double -> Double
pythagoras x y = sqrt (square x + square y)
  where
    square : Double -> Double
    square x = x * x
