module Day5 where
  
import Data.List.Split
import Data.Maybe
import Data.Map as M
import Control.Applicative

readInt :: String -> Int
readInt s = read s

newtype Addr = A { asValue :: Int }
  deriving (Eq, Ord, Show)
  
addP :: Addr -> Int -> Addr
addP (A p) off = A (p + off)
  
-- Store the program as a map
-- for quick random access
type Prog = M.Map Addr Int

getV :: Prog -> Addr -> Int
getV prog p = 
  case M.lookup p prog of
    Just x -> x
    Nothing -> error $ show p

getP :: Prog -> Addr -> Int -> Addr
getP prog p off = A $ fromJust $ M.lookup (addP p off) prog

putV :: Prog -> Addr -> Int -> Prog
putV prog p v = M.insert p v prog

mkProgram :: [Int] -> Prog
mkProgram listing = 
  let addressSpace = fmap A [0..]
  in M.fromList (zip addressSpace listing)
  
data Op = Plus | Times | In | Out | JumpT | JumpF | IsLess | IsEq | Stop
  deriving (Eq, Enum, Show)
  
data Mode = Ref | Imm 
  deriving (Eq, Enum, Show)
  
-- Could use the state monad, but it's an overkill

data Computer = Comp { output  :: [Int]
                     , ip      :: Addr
                     , program :: Prog
                     }
                     
mkComputer :: Prog -> Computer
mkComputer prog = Comp [] (A 0) prog
  

-- The two lists contain offsets (from the IP) of inputs and outputs
-- The lists may be empty, meaning no input or no output
decode :: Int -> (Op, [Mode], [Int], [Int])
decode n =
    let op = opCode n
    in (op, modes n, fst (inout op), snd (inout op))
  where
    inout :: Op -> ([Int], [Int])
    inout op = case op of 
      Plus  -> ([1, 2], [3])
      Times -> ([1, 2], [3])
      In    -> ([],     [1])
      Out   -> ([1],    [])
      JumpT -> ([1, 2], [])
      JumpF -> ([1, 2], [])
      IsLess-> ([1, 2], [3])
      IsEq  -> ([1, 2], [3])
      Stop  -> ([],     [])
    opCode :: Int -> Op
    opCode x = 
      if x == 99 
      then Stop
      else toEnum $ x `mod` 100 - 1
    modes :: Int -> [Mode]
    modes x = fmap digitToMode [ x `div` 100
                               , x `div` 1000
                               , x `div` 10000 ]
    digitToMode :: Int -> Mode
    digitToMode = toEnum . fromEnum . odd
   
-- First argument is external input
exec :: Int -> Computer -> Maybe Computer
exec i (Comp o ip prog) =
    let (opCode, modes, inOffs, outOffs) = decode $ getV prog ip
        ins  = fmap (getP prog ip) inOffs
        outs = fmap (getP prog ip) outOffs
        args = fmap getM $ zip modes ins
        (o', mip, prog') = step opCode args outs
        -- If no jump, increment IP
        newIp = fromMaybe (addP ip (length inOffs + length outOffs + 1)) mip
    in if opCode == Stop 
       then Nothing 
       else
         Just (Comp o' newIp prog')
  where 
    -- takes opcode, values of inputs, and addresses of outputs (zero or one)
    -- returns new external output list, maybe new IP (if jump), and new program
    step :: Op -> [Int] -> [Addr] -> ([Int], Maybe Addr, Prog)
    step op args outs =
      case op of
        Plus  -> ( o
                 , Nothing
                 , putV prog (head outs) (sum $ args))
        Times -> ( o
                 , Nothing
                 , putV prog (head outs) (product $ args))
        In    -> ( o
                 , Nothing
                 , putV prog (head outs) i)
        Out   -> ( (head args) : o
                 , Nothing
                 , prog)
        JumpT -> ( o
                 , if head args /= 0 then Just $ A $ args!!1 else Nothing
                 , prog)
        JumpF -> ( o
                 , if head args == 0 then Just $ A $ args!!1 else Nothing
                 , prog)
        IsLess-> ( o
                 , Nothing
                 , putV prog (head outs) 
                              (if args!!0 <  args!!1 then 1 else 0))
        IsEq  -> ( o
                 , Nothing
                 , putV prog (head outs) 
                              (if args!!0 == args!!1 then 1 else 0))
    getM :: (Mode, Addr) -> Int
    getM (m, p) = 
        if m == Ref 
        then getV prog p
        else asValue p

-- takes input, returns list of outputs
run :: Prog -> Int -> [Int]
run prog i = go (mkComputer prog)
  where go comp = case exec i comp of
                    Nothing -> output comp
                    Just comp' -> go comp'


main = do
  text <- readFile "Data5.txt"
  --let text = test'
  let listing = fmap readInt $ splitOn "," text
  let prog = mkProgram listing
  print $ run prog 5
  
test = "1002,4,3,4,33"
test''="3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
test' = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
