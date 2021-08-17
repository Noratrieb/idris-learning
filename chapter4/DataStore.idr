module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size items) = size

items : (store : DataStore) -> Vect (size store) String
items (MkData size items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

search : DataStore -> String -> List String
search store query = let predicate = isInfixOf query
                         allItems = toList (items store) in -- can't be bothered to work with dependant pairs
                     filter predicate allItems


data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" args = Just (Add args)
parseCommand "get" val = case all isDigit (unpack val) of
                               True => Just (Get (cast val))
                               False => Nothing
parseCommand "quit" args = Just Quit
parseCommand "size" args = Just Size
parseCommand "search" args = Just (Search args)
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)


getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                                  case integerToFin pos (size store) of
                                        Nothing => Just ("Out of range\n", store)
                                        Just idx => Just (index idx store_items ++ "\n", store)


processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("invalid command\n", store)
                              Just (Add item) =>
                                  Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              Just (Get pos) => getEntry pos store
                              Just Size => Just ("Size: " ++ show (size store) ++ "\n", store)
                              Just (Search query) => case (search store query) of
                                                        [] => Just ("Item Not Found\n", store)
                                                        items => Just (show items ++ "\n", store)
                              Just Quit => Nothing


main : IO ()
main = replWith (MkData _ []) "Command: " processInput
