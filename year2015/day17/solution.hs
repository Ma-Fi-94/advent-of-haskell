-- The list of all sublists of a list
sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = (map (x:) (sublists xs)) ++ (sublists xs)

main = do
    fileContent <- readFile "input.txt"
    let containers = map (read :: String -> Int) . lines $ fileContent
    let validCombinations = filter (\l -> sum l == 150) . sublists $ containers
        
    -- Part 1
    print $ length validCombinations
    
    -- Part 2
    let minNb = minimum . map length $ validCombinations
    print $ length . filter (\l -> length l == minNb) $ validCombinations

    print $ "--- Finished. ---"
