import Data.Maybe
import Utils (tok)

data CPU = CPU {ra :: Int,
                rb :: Int,
                ip :: Int} deriving (Show)

data Command =   HLF Char
               | TPL Char
               | INC Char
               | JMP Int
               | JIE Char Int
               | JIO Char Int  deriving (Show)

-- Update CPU state due to executing a single command
exec :: CPU -> Command -> CPU
exec (CPU ra rb ip) (HLF 'a') = (CPU (ra `div` 2) rb (ip+1))
exec (CPU ra rb ip) (HLF 'b') = (CPU ra (rb `div` 2) (ip+1))
exec (CPU ra rb ip) (TPL 'a') = (CPU (2*ra) rb (ip+1))
exec (CPU ra rb ip) (TPL 'b') = (CPU ra (2*rb) (ip+1))
exec (CPU ra rb ip) (INC 'a') = (CPU (ra+1) rb (ip+1))
exec (CPU ra rb ip) (INC 'b') = (CPU ra (rb+2) (ip+1))
exec (CPU ra rb ip) (JMP o)   = (CPU ra rb (ip+o))
exec (CPU ra rb ip) (JIE r o)
    |r == 'a' = if (even ra) then (CPU ra rb (ip+o)) else (CPU ra rb (ip+1))
    |r == 'b' = if (even rb) then (CPU ra rb (ip+o)) else (CPU ra rb (ip+1))
exec (CPU ra rb ip) (JIO r o)
    |r == 'a' = if (odd ra) then (CPU ra rb (ip+o)) else (CPU ra rb (ip+1))
    |r == 'b' = if (odd rb) then (CPU ra rb (ip+o)) else (CPU ra rb (ip+1))

-- One step
step :: [Command] -> CPU -> Maybe CPU
step cmds cpu@(CPU ra rb ip) = if (ip < length cmds)
                               then Just (exec cpu (cmds!!ip))
                               else Nothing


main :: IO ()
main = do
    fileContent <- readFile "test.txt"
    
    -- Part 1
    
    -- Part 2
    
    print $ "--- Finished. ---"

