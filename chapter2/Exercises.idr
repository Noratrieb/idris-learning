module Exercises

-- 1, 2, 3, 4, 5
palindrome : Nat -> String -> Bool
palindrome n str = let halfs = splitHalf (toLower str) in
                    length str > n && fst halfs == (reverse (snd halfs))
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

-- 6
counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

-- 7
top_ten : Ord a => List a -> List a
top_ten xs = take 10 (reverse (sort xs))

-- 8
over_length : Nat -> List String -> Nat
over_length n xs = let filtered = filter (\str => length str > n) xs in
                       length filtered

-- 9
-- not now
