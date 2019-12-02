{-# language DeriveFunctor #-}

module Day2 where
  
import GHC.Exts (groupWith)
import Data.List
import qualified Data.Map as M

main = do
  txt <- readFile "2.txt"
  let cs = lines txt
  print $ checkSum cs
  putStrLn "Brute force:"
  print $ findMatch cs
  putStrLn "Using Trie:"
  print $ findMatch1 cs
  putStrLn "Using hylo:"
  print $ head $ snd $ hylo accum fromList cs
  print $ length $ fst $ hylo accum fromList cs
  
-- Calculate the checksum

checkSum :: [String] -> Int
checkSum cs =
  let xs = fmap twoThree cs
      (twos, threes) = foldl 
        (\(x, x') (b, b') -> (x + fromBool b, x' + fromBool b'))
        (0, 0) xs
  in twos * threes
  
-- Define a multiset

type Counts a = M.Map a Int

add :: Ord a => Counts a -> a -> Counts a
add cs c = M.insertWith (+) c 1 cs

charCounts :: String -> Counts Char
charCounts s = foldl add M.empty s

twoThree :: String -> (Bool, Bool)
twoThree s = 
  let xs = M.elems (charCounts s)
  in (elem 2 xs, elem 3 xs)
  

-- Brute force solution
  
distance :: (String, String) -> Int
distance = sum . fmap fromBool . uncurry (zipWith (/=))

fromBool :: Num a => Bool -> a
fromBool False = 0
fromBool True  = 1

findMatch :: [String] -> String
findMatch ss = 
  let ps = [(s1, s2) | s1 <- ss, s2 <- ss]
      (s, s') = head $ filter ((== 1) . distance) ps
  in fmap fst $ filter (uncurry (==)) $ zip s s'
  
-- Using a Trie
  
data Trie = Trie [(Char, Int, Trie)]
  deriving (Show, Eq)

insertS :: Trie -> String -> Trie
insertS t "" = t
insertS (Trie bs) s = Trie (inS bs s)
  where
    inS ((x, n, t) : bs) (c : cs) =
      if c == x 
      then (c, n + 1, insertS t cs) : bs
      else (x, n, t) : inS bs (c : cs)
    inS [] (c : cs) = [(c, 1, insertS (Trie []) cs)]
  
mkTrie :: [String] -> Trie
mkTrie = foldl insertS (Trie [])

match1 :: Trie -> [String]
match1 (Trie bs) = go bs
  where
    go :: [(Char, Int, Trie)] -> [String]
    go ((x, n, t) : bs) = 
      let a1s = if n > 1 
                then fmap (x:) $ match1 t
                else []
          a2s = foldMap (exact t) bs
          a3s = go bs -- recurse over list
      in a1s ++ a2s ++ a3s
    go [] = []
    exact t (_, _, t') = matchAll t t'
  

-- Find all perfect matches between two Tries  
matchAll :: Trie -> Trie -> [String]
matchAll (Trie bs) (Trie bs') = mAll bs bs'
  where
    mAll :: [(Char, Int, Trie)] -> [(Char, Int, Trie)] -> [String]
    mAll [] [] = [""]
    mAll bs bs' = 
      let ps = [ (c, t, t') 
               | (c,  _,  t)  <- bs
               , (c', _', t') <- bs'
               , c == c']

      in foldMap go ps
    go (c, t, t') = fmap (c:) (matchAll t t')


findMatch1 :: [String] -> String
findMatch1 cs = head $ match1 (mkTrie cs)

-- Recursion schemes

type Algebra f x = f x -> x

type Coalgebra f x = x -> f x

newtype Fix f = In { out :: f(Fix f) }

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . out

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coa = In . fmap (ana coa) . coa

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo alg coa = alg . fmap (hylo alg coa) . coa

---

data TrieF a = TrieF [(Char, a)]
  deriving (Show, Functor)
  
-- Define the coalgebra
-- Seed is a list of strings

fromList :: Coalgebra TrieF [String]
fromList ss =
  -- are strings empty? (checking one is enough)
  if null (head ss) 
  then TrieF []
  else
    let sss = groupWith head ss
    in TrieF $ fmap mkBranch sss
  
mkBranch :: [String] -> (Char, [String])
mkBranch sss =
  let c = head (head sss)
  in (c, fmap tail sss)

showAlg :: Algebra TrieF [String]
showAlg (TrieF []) = []
showAlg (TrieF bs) = fmap (\(c, ss) -> [c] ++ offset ss) bs
  where offset ss = concat $ fmap (\s -> "-" ++ s ++ ".") ss
  
showTrie :: Fix TrieF -> String
showTrie = concat . cata showAlg

-- Define the algebra
-- Accumulatir is a pair of lists of strings

accum :: Algebra TrieF ([String], [String])
accum (TrieF []) = ([""], [])
accum (TrieF bs) = -- b :: (Char, ([String], [String]))
    let -- prepend chars to string in both lists
        pss = unzip $ fmap prep bs
        (ss1, ss2) = both concat pss
        -- find duplicates
        ss = concat $ fmap (fst . snd) bs
        mset = foldl add M.empty ss
        dups = M.keys $ M.filter (> 1) mset
     in (ss1, dups ++ ss2)
  where
      prep :: (Char, ([String], [String])) -> ([String], [String])
      prep (c, pss) = both (fmap (c:)) pss

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)
