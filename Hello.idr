module Main
main : IO ()
main = putStrLn "hello world"

StringOrInt : Bool -> Type
StringOrInt x = case x of
                    True => Int
                    False => String


getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt x = case x of
                        True => 94
                        False => "Ninety four"

TypeChooser : String -> Type
TypeChooser x = case x of
  "Int" => Int
  "Bool" => Bool
  "String" => String
  "Char" => Char

valToString : (typeChoice : Bool) -> StringOrInt typeChoice -> String
valToString typeChoice val = case typeChoice of
                          True => cast val  -- True means that our argument type is Int, so cast
                          False => val -- False means that our agumet type is String, so no need to cast
