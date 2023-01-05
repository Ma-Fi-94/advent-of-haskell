import Data.Word (Word16)
import Data.Bits
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Utils (tok)

import Debug.Trace

-- Parse an input line
parseLine :: String -> (String, Wire)
parseLine s = (name, wire)
  where
    tokens  = tok " ->" s
    name    = last tokens
    
    parse s = if all isDigit s
              then (Num (read s))
              else (Name s)
    
    wire    = case (length tokens) of
        2 -> parse (tokens!!0)
        3 -> Not (parse (tokens!!1))
        4 -> case (tokens!!1) of
                  "AND"    -> And    (parse (tokens!!0)) (parse (tokens!!2))
                  "OR"     -> Or     (parse (tokens!!0)) (parse (tokens!!2))
                  "RSHIFT" -> Rshift (read (tokens!!2))  (parse (tokens!!0))
                  "LSHIFT" -> Lshift (read (tokens!!2))  (parse (tokens!!0))
  
-- Wire type
data Wire = Num Word16
          | Name String
          | And Wire Wire
          | Or Wire Wire
          | Not Wire
          | Lshift Int Wire
          | Rshift Int Wire deriving (Show, Eq)



evalByName ::  Map.Map String Wire -> String -> Word16
evalByName defs name = case wire of
    (Num i)      -> i
    (Name s)     -> evalByName defs s
    (And w1 w2)  -> (evalWire w1) .&. (evalWire w2)
    (Or w1 w2)   -> (evalWire w1) .|. (evalWire w2)
    (Not w)      -> complement $ evalWire w
    (Lshift i w) -> shiftL (evalWire w) i
    (Rshift i w) -> shiftR (evalWire w) i
  where
    wire       = fromJust $ Map.lookup name defs
    evalWire w = case w of
                      (Num n)  -> n
                      (Name s) -> evalByName defs s

main = do
    fileContent <- readFile "test.txt"

    -- Part 1
    let defs = Map.fromList . map parseLine . lines $ fileContent
    print $ evalByName defs "x"
    
    -- Part 2
   
    print $ "--- Finished. ---"
