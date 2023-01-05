import Data.Char (isDigit)
import Utils (dropWhileIncl)

-- Sugar
type Array a   = [[a]]
type Coord     = (Int, Int) -- Zero-based (row, column)
type Command a = ((a->a), Coord, Coord)

-- Parse a single line of input
parseLine :: String -> Command Bool
parseLine s = (f, (x1,y1), (x2,y2))
  where
    f      = case (takeWhile (not . isDigit) s) of
                 "turn on "  -> (||True)
                 "turn off " -> (&&False)
                 "toggle "   -> (not)
    coords = dropWhile (not . isDigit) s
    x1     = read . takeWhile isDigit $ coords
    y1     = read . takeWhile isDigit . dropWhileIncl (/= ',') $ coords
    x2     = read . takeWhile isDigit . dropWhileIncl (/= ' ') . dropWhileIncl (/= ' ') $ coords
    y2     = read . takeWhile isDigit . dropWhileIncl (/= ',') . dropWhileIncl (/= ' ') $ coords

-- Parse a single line of input
parseLine2 :: String -> Command Int
parseLine2 s = (f, (x1,y1), (x2,y2))
  where
    f      = case (takeWhile (not . isDigit) s) of
                 "turn on "  -> (+1)
                 "turn off " -> (\d -> maximum [d-1,0])
                 "toggle "   -> (+2)
    coords = dropWhile (not . isDigit) s
    x1     = read . takeWhile isDigit $ coords
    y1     = read . takeWhile isDigit . dropWhileIncl (/= ',') $ coords
    x2     = read . takeWhile isDigit . dropWhileIncl (/= ' ') . dropWhileIncl (/= ' ') $ coords
    y2     = read . takeWhile isDigit . dropWhileIncl (/= ',') . dropWhileIncl (/= ' ') $ coords

-- Apply f to a rectangular region of an array, given by coordinates (borders included)
-- Parameter ordering deviates from regular map for easier fold
mapRect :: Array a -> ((a->a), Coord, Coord) -> Array a
mapRect ls (f,(x1,y1),(x2,y2)) = preLines ++ (map mapLine midLines) ++ postLines
  where
    preLines  = take x1 ls
    postLines = drop (x2+1) ls
    midLines  = take (x2-x1+1) . drop x1 $ ls
    mapLine l = preCols ++ midCols ++ postCols
      where
        preCols  = take y1 l
        postCols = drop (y2+1) l
        midCols  = map f (take (y2-y1+1) (drop y1 l))

main = do
    fileContent <- readFile "input.txt"

    -- Part 1
    let initArray = replicate 1000 (replicate 1000 False)
    let commands = map parseLine . lines $ fileContent 
    print $ sum . map fromEnum . concat . foldl mapRect initArray $ commands
    
    -- Part 2
    let initArray = replicate 1000 (replicate 1000 0)
    let commands = map parseLine2 . lines $ fileContent 
    print $ sum . concat . foldl mapRect initArray $ commands
   
    print $ "--- Finished. ---"
