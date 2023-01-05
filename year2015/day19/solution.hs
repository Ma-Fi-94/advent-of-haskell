import Utils (tok, at)
import Data.List (nub, isPrefixOf)
import Data.Char (isUpper)

-- Sugar
type Rule = (String, String)

-- Apply a replacement rule to an input string at all possible positions
apply :: String -> Rule -> [String]
apply [] _                = []
apply str rule@(old, new) = go 0
  where
    go i
        |i > length str                = []
        |old `isPrefixOf` (drop i str) = str' : go (i+1)
        |otherwise                     = go (i+1)
          where
            str' = (take i str) ++ new ++ (drop (i + length old) str)

-- Count the number of occurences of a sublist
countOcc :: (Eq a) => [a] -> [a] -> Int
countOcc hay ndl = go hay
  where
    go [] = 0
    go h
        |ndl `isPrefixOf` h = 1 + go (tail h)
        |otherwise          = go (tail h)
        
-- Apply all rules to all strings, return all possible new strings
step :: [Rule] -> [String] -> [String]
step rules strings = nub . concat . concat . map applyAll $ strings
  where
    applyAll = \string -> map (apply string) rules

-- Parse a rule input string
parseRule :: String -> Rule
parseRule str = (old, new)
  where
    tokens        = tok " =>" str
    (old, new)    = (tokens!!0, tokens!!1)

main = do
    fileContent   <- readFile "input.txt"
    let fileLines = lines fileContent
    let rules     = map parseRule . take (length fileLines - 2) $ fileLines
    let pattern   = last fileLines 

    -- Part 1
    print $ length . step rules $ [pattern]

    -- Part 2
    -- A brute force approach would look like:
    -- print $ fst . head . dropWhile (not . (pattern `elem`) . snd) . zip [0..] . iterate (step rules) $ ["e"]
    -- This, however explodes extremely fast, so it's not a feasible approach.
    
    -- Thus, consider the substitution rules and notice that any "atom"
    -- will always be substituted with one of the following:
    -- (1) {an atom} {an atom}
    -- (2) {an atom} Rn {an atom} Ar
    -- (3) {an atom} Rn {an atom} Y {an atom} Ar
    -- (4) {an atom} Rn {an atom} Y {an atom} Y {an atom} Ar

    -- Note that the beginning "e" already yields two atoms in the first step.
    -- Every subsequent step will then add one atom (rule type (1) and (2)),
    -- add two atoms (rule type (3)) or add three atoms (rule type (4)).
    -- We thus know that every step adds at least one atom, plus as many more
    -- atoms as Y's.

    -- Accordingly, we count the number of atoms in the final string, reduce
    -- this number by the number of Rn's and Ar's, as these are "on top",
    -- further decrease by twice the number of Y's, and subtract 1.

    let nbAll = length . filter isUpper $ pattern
    let nbRn  = countOcc pattern "Rn"
    let nbAr  = countOcc pattern "Ar"
    let nbY  = countOcc pattern "Y"
    print $ nbAll - nbRn - nbAr - 2*nbY - 1
    
    print $ "--- Finished. ---"
