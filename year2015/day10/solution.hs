import Data.List (group)

step :: [Char] -> [Char]
step s = concat . map (\l -> (show (length l)) ++ [head l]) . group $ s

main = do
    let input = "1113122113"

    -- Part 1
    print $ length ((iterate step input) !! 40)

    -- Part 2
    print $ length ((iterate step input) !! 50)

    print $ "--- Finished. ---"
