module Average

import Data.String


||| Calculate the average word length of a string
||| @str a string containing words seperated by whitespace
export
average : (str : String) -> Double
average str = let totalLength = sum (wordLengths str)
                  wordAmount = wordCount str in
                  cast totalLength / cast wordAmount
  where
    wordCount : String -> Nat
    wordCount str = length (words str)
    wordLengths : String -> List Nat
    wordLengths str = map length (words str)
