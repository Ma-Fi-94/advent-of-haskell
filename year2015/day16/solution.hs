import Utils (strip, searchFirst, at)
import Data.Maybe (fromJust)

-- Query the list of inputs to find all lines matching the provided specs.
query :: [(Int -> Bool)] -> [(Int, [Int])] -> [(Int, [Int])]
query specs = filter (fits specs . snd)

-- Check whether a line of input matches the specs
fits :: [(Int -> Bool)] -> [Int] -> Bool
fits specs = all (\(p,i) -> (p i) || i==(-1)) . zip specs

-- Parse a single line of input
-- Empty fields of the input are denoted with (-1)
parseLine :: String -> (Int, [Int])
parseLine l = (read (at 1 tokens), map lookup fields)
  where
    fields      = ["children", "cats", "samoyeds", "pomeranians", "akitas",
                   "vizslas", "goldfish", "trees", "cars", "perfumes"]
    tokens      = map strip . words $ l
    lookup t    = if   t `elem` tokens
                  then read $ tokens !! (1 + (fromJust (searchFirst t tokens)))
                  else -1

main = do
    fileContent <- readFile "input.txt"
    let fileLines = lines fileContent
    
    -- Part 1
    let specs = [(==3),(==7),(==2),(==3),(==0),(==0),(==5),(==3),(==2),(==1)]
    print $ fst . head . query specs . map parseLine $ fileLines
    
    -- Part 2
    let specs = [(==3),(>7),(==2),(<3),(==0),(==0),(<5),(>3),(==2),(==1)]
    print $ fst . head . query specs . map parseLine $ fileLines

    print $ "--- Finished. ---"
