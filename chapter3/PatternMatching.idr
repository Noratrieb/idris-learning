module Main

surrond_with_braces : String -> String
surrond_with_braces x = "{" ++ x ++ "}"

allLengths : List String -> List Nat
allLengths strs = map length strs

invert : Bool -> Bool
invert True = False
invert False = True

describeList : Show a => List a -> String
describeList [] = "Empty"
describeList (x :: xs) = "Non-empty (" ++ show x ++ "), tail = " ++ show xs

showList : Show a => List a -> String
showList [] = "[]"
showList (x :: xs) = "[" ++ show x ++ showListInner xs
  where
    showListInner : Show a => List a -> String
    showListInner [] = "]"
    showListInner (x :: xs) = ", " ++ show x ++ showListInner xs

allLengthsNoMap : List String -> List Nat
allLengthsNoMap [] = []
allLengthsNoMap (word :: words) = length word :: allLengthsNoMap words

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

isEven2 : Nat -> Bool
isEven2 Z = True
isEven2 (S k) = not (isEven2 k)

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k
  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k




displayList : Show a => List a -> String
displayList xs = "[" ++ showItems xs ++ "]"
  where
    showItems : Show a => List a -> String
    showItems [] = ""
    showItems (x :: []) = show x
    showItems (x :: next) = show x ++ "," ++ showItems next
