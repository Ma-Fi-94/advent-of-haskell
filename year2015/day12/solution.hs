import Data.Char (isDigit)
import Utils (tok)
import Text.Parsec

-- Type definition of valid JSON
data JSON = Num Int | Str String | Obj [(String, JSON)] | Arr [JSON] deriving (Show, Eq)

-- Parse a JSON expression to a list of integers, removing all "red objects"
unred :: JSON -> [Int]
unred (Num i) = [i]
unred (Str s) = []
unred (Obj d) = if (Str "red") `elem` (map snd d)
                then []
                else concat $ map (unred . snd) d
unred (Arr l) = concat $ map unred l

-- Helper for parsing the successful result of Parsec
fromRight (Right a) = a

-- A valid JSON expression is either a str, a num, an obj, or an arr,
-- as defined below.
jsonExpr :: Parsec String u JSON
jsonExpr = str <|> num <|> obj <|> arr

-- Something is a 'quoted' if it is enclosed in double quotation marks
quoted :: Parsec String u String
quoted = char '"' *> many (noneOf ['"']) <* char '"'

-- A JSON string is anything that is quoted.
str :: Parsec String u JSON
str = Str <$> quoted

-- A JSON number, either positive or negative
num :: Parsec String u JSON
num = (Num . negate . read <$> (char '-' *> many1 digit))
    <|>
      (Num . read <$> many1 digit)

-- A JSON object    
obj :: Parsec String u JSON
obj = do
    _ <- char '{'
    kvs <- (do
            key <- quoted <* spaces <* char ':' <* spaces
            value <- jsonExpr
            return (key,value)
            ) `sepBy` (spaces *> char ',' <* spaces)
    _ <- char '}'
    return $ Obj kvs

-- A JSON array
arr :: Parsec String u JSON
arr = do
    _ <- char '['
    val <- jsonExpr `sepBy` (spaces *> char ',' <* spaces)
    _ <- char ']'
    return $ Arr val



main = do
    fileContent <- readFile "input.txt"
    let input = head . lines $ fileContent
    
    -- Part 1
    let delims = "[]{},:\""
    let digits = "0123456789-"
    print $ sum . map read . filter (/="") . filter (all (`elem` digits)) . tok delims $ input

    -- Part 2
    -- The entirety of part 2 is heavily based with only minor edits on
    -- reddit.com/r/adventofcode/comments/3wh73d/comment/cxw8uto/
    print $ sum . unred . fromRight . parse jsonExpr "inputnamedoesntmatter" $ fileContent


    print $ "--- Finished. ---"
