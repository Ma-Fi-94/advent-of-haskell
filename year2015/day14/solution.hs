import Data.List (elemIndices, sort, group)

-- Sugar
type Time     = Int
type Velocity = Int
type R        = (Velocity, Time, Time)

-- Parse a single line of input
parseLine :: String -> R
parseLine l = (read ((words l)!!3),
               read ((words l)!!6),
               read ((words l)!!13))

-- Distance travelled after t seconds
dist :: Time -> R -> Int
dist t (v,t1,t2) = v * t1 * (t `div` (t1+t2)) + v * minimum [t1, t `mod` (t1+t2)]

-- Calculate who has the lead after t seconds
lead :: [R] -> Time -> [Int]
lead rs t = elemIndices (maximum dists) dists
  where
    dists = map (dist t) rs 

main = do
    fileContent <- readFile "input.txt"
    let rs = map parseLine . lines $ fileContent    

    -- Part 1
    print $ maximum . map (dist 2503) $ rs

    -- Part 2
    print $ maximum . map length . group . sort . concat . map (lead rs) $ [1..2503]
    
    print $ "--- Finished. ---"
