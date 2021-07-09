module Matrix

import Data.Vect

addMatrix : Num numType =>
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType)
addMatrix = zipWith (zipWith (+))

multMatrix : Num numType =>
              Vect n (Vect m numType) -> Vect m (Vect p numType) ->
              Vect n (Vect p numType)
-- 😔

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

createEmpties2 : Vect n (Vect 0 elem)
createEmpties2 {n = Z} = []
createEmpties2 {n = (S k)} = [] :: createEmpties2


transHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
transHelper = zipWith (\x, y => x :: y)

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                           transHelper x xsTrans
