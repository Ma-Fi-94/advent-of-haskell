import Utils (at, ats, trimLo)
import Data.List (transpose)

type Amount   = Int
type Property = Int

-- Generate all possible ways to combine n>1 numbers to sum up to t
combos :: Int -> Int -> [[Int]]
combos n t = go 1 $ map (\i->[i]) [0..t]
  where
    go step lists
        |step == (n-1) = map (\l -> l ++ [t-(sum l)]) lists
        |otherwise     = go (step+1) lists'
          where
            lists' = concat . map (\l -> [l++[x] | x <- [0..(t-(sum l))]]) $ lists

-- Parse all input line by line
parseInput :: [Char] -> [[Property]]
parseInput = map parseLine . lines
  where
    parseLine = map read . ats [2,4,6,8,10] . map strip . words

-- Strip off all trailing whitespaces and punctuations
strip :: String -> String
strip s = reverse . stripHead . reverse . stripHead $ s
  where
    stripHead = dropWhile (`elem` " ,.;:")

-- Score a recipe
score :: [[Property]] -> [Amount] -> Int
score pss as = foldl1 (*) . map (trimLo 0 . sum) . transpose . mulVecMat as $ (map (take 4) pss)
  where
    mulVecMat v m = map (\(a, ps) -> map (*a) ps) $ zip v m

-- Score a recipe with the constraint of calories adding to exactly 500
score2 :: [[Property]] -> [Amount] -> Int
score2 pss as = if   kcal == 500
                then score pss as
                else -99999
  where
    kcal = sum $ zipWith (*) as (map (at 4) pss)

main = do
    fileContent <- readFile "input.txt"
    let properties = parseInput fileContent
    
    -- Part 1
    print $ maximum . map (score properties) . combos (length properties) $ 100

    -- Part 2
    print $ maximum . map (score2 properties) . combos (length properties) $ 100    

    print $ "--- Finished. ---"
