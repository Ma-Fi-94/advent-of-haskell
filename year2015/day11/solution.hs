import Data.List (group, nub)
import Data.Char (ord, chr)
import Utils (at)

-- Check whether a proposed password is valid
valid :: String -> Bool
valid s = (hasStraight s) && (noIllegal s) && (hasPairs s)
  where
    hasStraight (c1:c2:c3:cs)
        |cs == []  = valid (c1,c2,c3)
        |otherwise = valid (c1,c2,c3) || hasStraight (c2:c3:cs)
          where
            valid (c1,c2,c3) = (ord c1 == (ord c2)-1) && ord c2 == (ord c3)-1
    
    noIllegal s = and . map not . map (`elem` ['i','o','l']) $ s
    
    hasPairs s  = length (pairedChars s) >= 2
      where
        pairedChars s = nub . map head . filter (\l -> length l == 2) . group $ s

-- Increment a lower-case string
inc :: String -> String
inc s = go (reverse s) True
  where
    go ""     False = ""
    go ""     True  = "a"
    go (c:cs) carry = go cs carry' ++ [curChr]
      where
        curOrd           = (ord c) + (fromEnum carry)
        (curChr, carry') = if curOrd <= ord 'z'
                           then (chr curOrd, False)
                           else (chr (curOrd - (ord 'z') + (ord 'a') - 1), True)

main = do
    let input = "cqjxjnds"
    let passwords = iterate inc input
    
    -- Part 1
    print $ fst . head . filter snd . zip passwords $ (map valid passwords)

    -- Part 2
    print $ fst . at 1 . filter snd . zip passwords $ (map valid passwords)

    print $ "--- Finished. ---"
