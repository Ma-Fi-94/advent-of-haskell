import Data.List (nub)
import Utils (groupn)

-- Sugar
type Coord = (Int, Int)

parseFile :: String -> [Char]
parseFile = head . lines

-- Compute new coordinates for a direction character
move :: Coord -> Char -> Coord
move (x,y) c = case c of {'^'->(x,y+1); 'v'->(x,y-1); '<'->(x-1,y); '>'->(x+1,y)}

-- The same, but for Santa and Robo-Santa in parallel
move2 :: [Coord] -> [Char] -> [Coord]
move2 ((x1,y1):(x2,y2):[]) (c1:c2:[]) = ((x1',y1'):(x2',y2'):[])
  where
    (x1', y1') = move (x1,y1) c1
    (x2', y2') = move (x2,y2) c2

main = do
    fileContent <- readFile "input.txt"
    let input = parseFile fileContent
    
    -- Part 1
    print $ length . nub . scanl move (0,0) $ input 

    -- Part 2
    print $ length . nub . concat . scanl move2 [(0,0), (0,0)] . groupn 2 $ input
    
   

    print $ "--- Finished. ---"

