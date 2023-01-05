import Utils (none,containsSublist)

nice :: String -> Bool
nice s = (nbVowels >= 3) && hasDoublet && noForbidden
  where
    nbVowels    = length . filter (`elem` "aeiou") $ s
    twomerList  = map (\(a,b) -> [a,b]) $ zip s (tail s)
    hasDoublet  = any (\l -> (l!!0)==(l!!1)) $ twomerList
    noForbidden = none $ map (`elem` ["ab","cd","pq","xy"]) twomerList

nice2 :: String -> Bool
nice2 s = go1 s && go2 s
  where
    go1 (c1:c2:[])    = False
    go1 (c1:c2:cs)    = containsSublist (c1:[c2]) cs || go1 (c2:cs)
    
    go2 (c1:c2:[])    = False
    go2 (c1:c2:c3:cs) = (c1==c3) || go2 (c2:c3:cs)

main = do
    fileContent <- readFile "input.txt"
    let input = lines $ fileContent
    
    -- Part 1
    print $ sum . map (fromEnum . nice) $ input


    -- Part 2
    print $ sum . map (fromEnum . nice2) $ input 
   

    print $ "--- Finished. ---"
