import Data.Maybe
import Utils (tok, at)
import Debug.Trace

data CPU = CPU {ra :: Int,
                rb :: Int,
                ip :: Int} deriving (Show)

data Command =   HLF Char
               | TPL Char
               | INC Char
               | JMP Int
               | JIE Char Int
               | JIO Char Int  deriving (Show)

-- Parse one input line into a command
parseLine :: String -> Command
parseLine s = case tok0 of
    "hlf" -> HLF $ tokToReg tok1
    "tpl" -> TPL $ tokToReg tok1
    "inc" -> INC $ tokToReg tok1
    "jmp" -> JMP $ tokToOffset tok1
    "jie" -> JIE (tokToReg tok1) (tokToOffset tok2)
    "jio" -> JIO (tokToReg tok1) (tokToOffset tok2)
  where
    (tok0, tok1, tok2) = (tokens!!0, tokens!!1, tokens!!2)
    tokens             = tok " ," s
    tokToReg           = head
    tokToOffset        = read . dropWhile (=='+')

-- Update CPU state due to updating a single command
upd :: CPU -> Command -> CPU
upd (CPU ra rb ip) (HLF 'a') = (CPU (ra `div` 2) rb (ip+1))
upd (CPU ra rb ip) (HLF 'b') = (CPU ra (rb `div` 2) (ip+1))
upd (CPU ra rb ip) (TPL 'a') = (CPU (3*ra) rb (ip+1))
upd (CPU ra rb ip) (TPL 'b') = (CPU ra (3*rb) (ip+1))
upd (CPU ra rb ip) (INC 'a') = (CPU (ra+1) rb (ip+1))
upd (CPU ra rb ip) (INC 'b') = (CPU ra (rb+1) (ip+1))
upd (CPU ra rb ip) (JMP o)   = (CPU ra rb (ip+o))
upd (CPU ra rb ip) (JIE r o)
    |r == 'a' = if (even ra) then (CPU ra rb (ip+o)) else (CPU ra rb (ip+1))
    |r == 'b' = if (even rb) then (CPU ra rb (ip+o)) else (CPU ra rb (ip+1))
upd (CPU ra rb ip) (JIO r o)
    |r == 'a' = if ra==1 then (CPU ra rb (ip+o)) else (CPU ra rb (ip+1))
    |r == 'b' = if rb==1 then (CPU ra rb (ip+o)) else (CPU ra rb (ip+1))


-- Execute commands until we jump out of code.
-- We provide the initialisation of register A as first arg.
run :: Int -> [Command] -> CPU
run ra cmds = go (CPU ra 0 0)
  where
    go cpu@(CPU ra rb ip) = if   ip < length cmds
                            then go (upd cpu (cmds!!ip))
                            else cpu
        
main :: IO ()
main = do
    fileContent <- readFile "input.txt"
    let commands = map parseLine . lines $ fileContent
    
    -- Part 1
    print $ rb . run 0 $ commands
    
    -- Part 2
    print $ rb . run 1 $ commands
    
    print $ "--- Finished. ---"

