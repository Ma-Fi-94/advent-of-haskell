parseFile :: String -> [Int]
parseFile = map parseChar . head . lines
  where
    parseChar c = case c of {'(' -> 1; ')' -> (-1)}

main = do
    fileContent <- readFile "input.txt"
    let input = parseFile fileContent
    
    -- Part 1
    print $ sum $ input

    -- Part 2
    print $ length . takeWhile (>=0) . scanl (+) 0 $ input

    print $ "--- Finished. ---"

