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
