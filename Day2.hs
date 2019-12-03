module Day2 where
  
import Data.List.Split
import Data.Maybe
import Data.Map as M
import Control.Applicative

readInt :: String -> Int
readInt s = read s

-- This may be an overkill
-- but we worship type safety
newtype Addr = A Int 
  deriving (Eq, Ord)
  
-- We could have derived Num
-- but this is safer
addP :: Addr -> Int -> Addr
addP (A p) off = A $ p + off
  
-- Store the program as a map
-- for quick random access
type Prog = M.Map Addr Int

get :: Addr -> Prog -> Int
get p prog = fromJust $ M.lookup p prog

getP :: Addr -> Int -> Prog -> Addr
getP p off prog = A $ fromJust $ M.lookup (addP p off) prog

put :: Addr -> Int -> Prog -> Prog
put p v prog = M.insert p v prog

mkProgram :: [Int] -> Prog
mkProgram listing = 
  let addressSpace = fmap A [0..]
  in M.fromList (zip addressSpace listing)
  
run :: Prog -> (Int, Int) -> Int
run prog (noun, verb) = 
  let prog1 = put (A 1) noun prog
      prog2 = put (A 2) verb prog1
  in runProg (A 0) prog2
  
runProg :: Addr -> Prog -> Int
runProg ip prog = 
   let opCode = get ip prog
   in if opCode == 99 
      then get (A 0) prog -- we're done!
      else 
        let p1 = getP ip 1 prog
            p2 = getP ip 2 prog
            pr = getP ip 3 prog
        in runProg (addP ip 4) (exec opCode p1 p2 pr prog)
      
exec :: Int -> Addr -> Addr -> Addr -> Prog -> Prog
exec op p1 p2 pr prog =
  let m = get p1 prog
      n = get p2 prog
      val = if op == 1 
            then m + n 
            else m * n
  in put pr val prog
  
makeOutput :: Int -> Prog -> Int
makeOutput out prog = 
   let pairs = (,) <$> [0..99] <*> [0..99]
       outputs = zip (fmap (run prog) pairs) pairs
       (noun, verb) = fromJust $ Prelude.lookup out outputs
   in 100 * noun + verb 
  
main = do
  text <- readFile "Data2.txt"
  let listing = fmap readInt $ splitOn "," text
  let prog = mkProgram listing
  print $ run prog (12, 2)
  print $ makeOutput 19690720 prog
  
test = [1,9,10,3,2,3,11,0,99,30,40,50]
