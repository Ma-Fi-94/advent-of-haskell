import qualified Data.Map as Map
import Data.List (nub, permutations)

-- Sugar
type Name  = String 
type Pair  = (Name, Name)
type Entry = (Pair, Int)
type Table = Map.Map Pair Int

-- Parse a single raw line of the input file
parseLine :: String -> Entry
parseLine s = case (words s) of
    [n1, _, "gain", i, _, _, _, _, _, _, n2] -> ((n1,init n2), read i)
    [n1, _, "lose", i, _, _, _, _, _, _, n2] -> ((n1,init n2), -(read i))     
    _                                        -> error "Malformed input string"

-- Parse all lines of the input file, returning a list of all names and
-- the happiness values per pairing
parseInput :: [String] -> ([Name], Table)
parseInput ls = (names, table)
  where
    table = Map.fromList $ map parseLine $ ls
    names = nub $ map fst . Map.keys $ table

-- Given a list of names which describe the circular seating arrangement,
-- get all pairs of adjacent person names
pairings :: [Name] -> [(Name, Name)]
pairings l = rights ++ lefts
  where
    rights = zip l ((tail l) ++ [head l])
    lefts  = zip l ([last l] ++ init l)


-- Given a happiness table, score a seating arrangement defined by name list
score :: Table -> [Name] -> Int
score t l = sum . map (t Map.!) $ (pairings l) 
  
main = do
    fileContent <- readFile "input.txt"
    let (names, table) = parseInput . lines $ fileContent
    
    -- Part 1
    let pairings = permutations names
    print $ maximum . map (score table) $ pairings 

    -- Part 2
    let pairings2 = permutations (names ++ ["I"])
    let entriesI  = Map.fromList $    [(("I", name), 0) | name <- names]
                                   ++ [((name, "I"), 0) | name <- names]
    let table2    = Map.union table entriesI
    print $ maximum . map (score table2) $ pairings2
    
    print $ "--- Finished. ---"
