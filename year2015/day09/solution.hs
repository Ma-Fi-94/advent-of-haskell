import qualified Data.Map as Map
import Data.List (sort, nub, permutations)

-- Sugar / Oh, honey, honey / You are my candy girl / etc.
type Node     = String
type Edge     = (Node, Node)
type Distance = Int
type DistMap  = Map.Map Edge Distance

-- Parse a single raw line from input file.
-- `words` and pattern matching makes for straightforward parsing
parseLine :: String -> [(Edge, Distance)]
parseLine s = case (words s) of
    [l1, "to", l2, "=", d] -> [((l1, l2), read d), ((l2,l1), read d)]
    _                      -> error "Malformed input string. Aborting."

-- Parse the lines of an input file, returning a list of all locations
-- and a map containing the length of every edge
parseLines :: [String] -> ([Node], DistMap)
parseLines ls = (nodes, distmap)
  where
    distmap = Map.fromList . concat . map parseLine $ ls
    nodes   = nub . map fst $ Map.keys distmap

-- Calculate the overall length of one route, given by a list of place names
len :: DistMap -> [Node] -> Distance
len dists (l1:l2:[]) = dists Map.! (l1,l2)
len dists (l1:l2:ls) = dists Map.! (l1,l2) + len dists (l2:ls) 

main = do
    fileContent <- readFile "input.txt"
    let (nodes, distmap) = parseLines $ lines fileContent
    let routes           = permutations nodes
    let lengths          = map (len distmap) routes

    -- Part 1
    print $ head . sort $ lengths
    
    -- Part 2
    print $ last . sort $ lengths
    
    print $ "--- Finished. ---"
