module Main
import Average

showAverage : String -> String
showAverage str = show (average str)

main : IO ()
main = repl "\n> " showAverage
