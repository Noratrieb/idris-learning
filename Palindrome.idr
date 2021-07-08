module Palindrome

palindrome : String -> Bool
palindrome str = let halfs = splitHalf str in
                     fst halfs == (reverse (snd halfs))
  where
    halfLength : String -> Nat
    halfLength str = (length str) `div` 2

    splitHalf : String -> (String, String)
    splitHalf str = let firstHalf = substr 0 (halfLength str) str
                        startIndex = if even (length str) then halfLength str else halfLength str + 1
                        secondHalf = substr startIndex (length str) str in
                        (firstHalf, secondHalf)
      where
        even : Nat -> Bool
        even n = n `mod` 2 == 0
