import Utils (tok)
import Data.List (sort)

parseFile :: String -> [(Int, Int, Int)]
parseFile = map (tuplify3Int . tok "x") . lines
  where
    tuplify3Int = \(a:b:c:[]) -> (read a, read b, read c)

area :: (Int, Int, Int) -> Int
area (a,b,c) = 2*a*b + 2*a*c + 2*b*c + minimum [a*b, a*c, b*c]

ribbon :: (Int, Int, Int) -> Int
ribbon (a,c,b) = 2*((sort [a,b,c])!!0 + (sort [a,b,c])!!1) + a*b*c

main = do
    fileContent <- readFile "input.txt"
    let input = parseFile fileContent
    
    -- Part 1
    print $ sum . map area $ input

    -- Part 2
    print $ sum . map ribbon $ input
   

    print $ "--- Finished. ---"

