module Brainfuck

import Data.Vect

data Memory = End
            | Value Memory Int Memory


emptyMemory : Nat -> Memory
emptyMemory Z = End
emptyMemory S(k) = Value 0 (emptyMemory k)
