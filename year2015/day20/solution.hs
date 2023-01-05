import Data.List (nub)

-- Optimised way of factoring an integer, making use of the symmetry of (*)
factors :: Int -> [Int]
factors n = nub . concat $ pairs
  where
    pairs = [[x, n `div` x] | x <- [1..isqrt(n)], n `mod` x == 0]
    isqrt = floor . sqrt . fromIntegral

-- Nb. of presents for a given house: ten times the sum of all divisors.
presents :: Int -> Int
presents n = (10*) . sum $ factors n

-- Nb. of presents for the second part.
-- We exclude all divisors which are too small.
presents2 :: Int -> Int
presents2 n = (11*) . sum . filter (> (n-1) `div` 50) . factors $ n

main :: IO ()
main = do
    let input = 34000000
    
    -- Part 1
    print $ (+1) . length . takeWhile (<input) . map presents $ [0..]
    
    -- Part 2
    print $ (+1) . length . takeWhile (<input) . map presents2 $ [0..]
    
    print $ "--- Finished. ---"

