import Utils (pad, map2D, at)

-- Sugar
type Array = [[Bool]]
type Coord = (Int, Int)

-- Get the sum of True values in the Moore neighbourhood of cell (r,c)
mooreSum :: Array -> Coord -> Int
mooreSum arr (r,c) = sum . map (fromEnum . getCell arr) $ neighbours
  where
    neighbours = [(r-1,c-1), (r-1,c), (r-1,c+1),
                  (r,  c-1),            (r,c+1),
                  (r+1,c-1), (r+1,c), (r+1,c+1)]

-- Get the content of an array cell
getCell :: Array -> Coord -> Bool
getCell arr (r,c) = (arr!!r)!!c

-- Transition function of a single tile
delta :: Array -> Coord -> Bool
delta arr coord = case getCell arr coord of
    True  -> mooreSum arr coord `elem` [2,3]
    False -> mooreSum arr coord == 3     

-- Propagate the board by one step
propagate :: Array -> Array
propagate arr = go (pad False arr) [] [] (1,1)
  where
    go a complLines curLine (r,c)
        |r == rmax && c > cmax = complLines ++ [curLine]
        |c <= cmax             = go a complLines (curLine ++ [delta a (r,c)]) (r,c+1)
        |c > cmax              = go a (complLines ++ [curLine]) [] (r+1,1)
          where
           rmax = (length a) - 2
           cmax = (length (a!!0)) - 2

-- Propagate, while keeping the corners active all the time
propagate2 :: Array -> Array
propagate2 = switchCorners . propagate

-- Switch corners on
switchCorners :: Array -> Array
switchCorners arr = [top'] ++ mid ++ [bottom']
  where
    (top, mid, bottom) = (head arr, (tail . init) arr, last arr)
    top'               = [True] ++ (tail . init) top ++ [True]
    bottom'            = [True] ++ (tail . init) bottom ++ [True]

main = do
    fileContent <- readFile "input.txt"
    let board = map2D (\c -> c=='#') . lines $ fileContent
      
    -- Part 1
    print $ sum . map fromEnum . concat . at 100 . iterate propagate $ board

    -- Part 2
    print $ sum . map fromEnum . concat . at 100 . iterate propagate2 $ switchCorners board
    
    print $ "--- Finished. ---"
