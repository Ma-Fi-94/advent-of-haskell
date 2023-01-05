module Utils where

-- Strip off all trailing whitespaces and punctuations
strip :: String -> String
strip s = reverse . stripHead . reverse . stripHead $ s
  where
    stripHead = dropWhile (`elem` " ,.;:")

-- Checks whether an ord-able eq-able is element of a closed interval
inC :: (Ord a, Eq a) => a -> (a,a) -> Bool
inC e (low, up) = (e >= low) && (e <= up)

-- Checks whether an ord-able eq-able is element of an openinterval
inO :: (Ord a, Eq a) => a -> (a,a) -> Bool
inO e (low, up) = (e > low) && (e < up)

-- Returns true iff all list elements are False
none :: [Bool] -> Bool
none xs = (or xs) == False

-- Mapping a function onto every inner list of a list of lists.
map2D :: (a->b) -> [[a]] -> [[b]]
map2D f = map (map f)


----------------------
-- Number wrangling --
----------------------
trim :: (Ord a, Num a) => a -> a -> a -> a
trim lo hi = trimHi hi . trimLo lo
 
trimLo :: (Ord a, Num a) => a -> a -> a
trimLo lo x = maximum [x, lo]

trimHi :: (Ord a, Num a) => a -> a -> a
trimHi hi x = minimum [x, hi]

--------------------
-- List wrangling --
--------------------

-- Tokenise a list with delimiters.
-- Delimiters are not part of the output, and
-- multiple delimiters are treated as a single one.
tok :: Eq a => [a] -> [a] -> [[a]]
tok _ [] = []
tok ds xs = token : tok ds rest
  where
    token = takeWhile (not . isDelim) xs
    rest = dropWhile isDelim $ dropWhile (not . isDelim) xs
    isDelim = (`elem` ds)

-- Get only the singleton elements of a list
singletons :: Eq a => [a] -> [a]
singletons [] = []
singletons (x:xs)
    |not (x `elem` xs) = x : singletons xs
    |otherwise         = singletons (filter (/=x) xs)

-- Like !!, but arguments reversed
at :: Int -> [a] -> a
at n xs = xs !! n

-- Get elements from a list, specified by indices
ats :: [Int] -> [a] -> [a]
ats [] xs = []
ats (i:is) xs = (at i xs) : (ats is xs)

-- Just like takeWhile, but include the first element that failed the check
takeWhileIncl :: (a -> Bool) -> [a] -> [a]
takeWhileIncl _ [] = []
takeWhileIncl p (x:xs)
    |p x       = x : takeWhileIncl p xs
    |otherwise = [x]

-- Just like dropWhile, but also drop the first element that failed the check
dropWhileIncl :: (a -> Bool) -> [a] -> [a]
dropWhileIncl p = drop 1 . dropWhile p

-- Takes every n-th element of a list
every :: Int -> [a] -> [a]
every n xs = case (drop (n-1) xs) of
    [] -> []
    (y:ys) -> y:(every n ys)

-- Takes a list and groups it into sublists of length n.
-- The last sublist will be shorter if input is not divisible without rest.
groupn :: Int -> [a] -> [[a]]
groupn _ [] = []
groupn n lst = (take n lst) : (groupn n (drop n lst))

-- Apply a function f to the i-th element of list xs
applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt i f xs
    |i < 0 || i >= length xs = error "utils.applyAt: Index out of bounds."
    |otherwise               = (take i xs) ++ [f (xs!!i)] ++ (drop (i+1) xs)

-- Remove the i-th element from list xs
removeAt :: Int -> [a] -> [a]
removeAt i xs
    |i < 0 || i >= length xs = error "utils.removeAt: Index out of bounds."
    |otherwise              = (take i xs) ++ (drop (i+1) xs)

-- Insert an element e at the i-th position of a list xs
insertAt :: Int -> a -> [a] -> [a]
insertAt i e xs
    |i < 0 || i > length xs = error "utils.removeAt: Index out of bounds."
    |otherwise              = (take i xs) ++ [e] ++ (drop i xs)

---------------
-- Searching --
---------------

-- Get first position of an element in a list of eq-ables
searchFirst :: Eq a => a -> [a] -> Maybe Int
searchFirst elem list  = go 0 list
  where
    go ctr []            = Nothing
    go ctr (x:xs)
        |(x == elem)     = Just ctr
        |otherwise       = go (ctr+1) xs 

-- Get positions of an element in a list of eq-ables
searchAll :: Eq a => a -> [a] -> [Int]
searchAll elem list = go 0 list
  where
    go ctr []            = []
    go ctr (x:xs)
        |(x == elem)     = ctr : go (ctr+1) xs
        |otherwise       = go (ctr+1) xs 

-- Get positions of ALL elements in a list of eq-ables
searchAlls :: Eq a => [a] -> [a] -> [Int]
searchAlls elems list = go 0 list
  where
    go ctr []            = []
    go ctr (x:xs)
        |(x `elem` elems)  = ctr : go (ctr+1) xs
        |otherwise       = go (ctr+1) xs 

-- Check for existence of a sublist in a list of eq-ables.
containsSublist :: Eq a => [a] -> [a] -> Bool
containsSublist ndl hay = go hay 0
  where
    go hay ctr
        | length ndl > length hay        = False
        | ndl == take (length ndl) hay   = True
        | otherwise                      = go (drop 1 hay) (ctr + 1)

-- Find the first position of a sublist in a list of eq-ables.
searchSublist :: Eq a => [a] -> [a] -> Maybe Int
searchSublist ndl hay = go hay 0
  where
    go hay ctr
        | length ndl > length hay        = Nothing
        | ndl == take (length ndl) hay   = Just ctr
        | otherwise                      = go (drop 1 hay) (ctr + 1)

------------
-- Tuples --
------------

-- Convenience functions for getting element from 3-tuples
fir :: (a,b,c) -> a
sec :: (a,b,c) -> b
thi :: (a,b,c) -> c
fir (x,_,_) = x
sec (_,x,_) = x
thi (_,_,x) = x

-- Convert a list of two elements to a 2-tuple
tuplify2 :: [a] -> (a,a)
tuplify2 (x1:x2:[]) = (x1,x2)
tuplify2 _ = error "tuplify2: List needs to have exactly 2 elements."

-- And the opposite
listify2 :: (a,a) -> [a]
listify2 (x,y) = [x,y]

-- Convert a list of two elements to a 2-tuple
tuplify3 :: [a] -> (a,a,a)
tuplify3 (x1:x2:x3:[]) = (x1,x2,x3)
tuplify3 _ = error "tuplify3: List needs to have exactly 3 elements."

-- And the opposite
listify3 :: (a,a,a) -> [a]
listify3 (x,y,z) = [x,y,z]

---------------
-- Functions --
---------------

-- Makes a small table of values for a function
tab :: (Num a, Enum a) => (a -> Int) -> [(a, Int)]
tab f = zip xs (map f xs) 
  where
    xs = [(-3)..3]
