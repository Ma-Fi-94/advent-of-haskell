import MD5

solve :: String -> Int -> Int
solve prefix i = go 0
  where
    go suffix
      |take i hash == replicate i '0' = suffix
      |otherwise                      = go (suffix+1)
        where
          hash = md5s (Str full)
          full = prefix ++ (show suffix)

main = do
    let input = "ckczppom"
    
    -- Part 1
    print $ solve input 5

    -- Part 2
    print $ solve input 6
    
    print $ "--- Finished. ---"

