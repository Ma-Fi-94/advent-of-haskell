-- Part 1
memLen :: String -> Int
memLen str = go 0 $ init (tail str)
  where
    go ctr [] = ctr
    go ctr s
        |take 2 s == "\\x" = go (ctr+1) (drop 4 s)
        |head s == '\\'    = go (ctr+1) (drop 2 s)
        |otherwise         = go (ctr+1) (tail s)

-- Part 2
encodeLen :: String -> Int
encodeLen str = 2 + (go 0 str)
  where
    go ctr [] = ctr
    go ctr (c:cs)
        |c `elem` ['"', '\\'] = go (ctr+2) cs
        |otherwise          = go (ctr+1) cs

main = do
    fileContent <- readFile "input.txt"
    let fileLines = lines $ fileContent

    -- Part 1
    print $ sum  . map (\l -> length l - memLen l) $ fileLines
    
    -- Part 2
    print $ sum  . map (\l -> encodeLen l - length l) $ fileLines
   
    print $ "--- Finished. ---"
