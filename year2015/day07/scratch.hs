-- Evaluate recursively a wire, given a map containing all the
-- wires' assigments
eval :: Map.Map String Wire -> Wire -> Word16
eval _    (Num i)      = i
eval defs (Name s)     = eval defs $ fromJust (Map.lookup s defs)
eval defs (And w1 w2)  = (eval defs w1) .&. (eval defs w2)
eval defs (Or w1 w2)   = (eval defs w1) .|. (eval defs w2)
eval defs (Not w)      = complement (eval defs w)
eval defs (Lshift i w) = shiftL (eval defs w) i
eval defs (Rshift i w) = shiftR (eval defs w) i   
