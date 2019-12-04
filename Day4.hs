module Day4 where
  
import Data.List

-- General purpose reverse digitizer
digitsR :: Integral a => a -> [a]
digitsR = fmap snd 
        . takeWhile (/= (0, 0)) 
        . iterate (divMod10 . fst) 
        . divMod10
  where divMod10 = flip divMod 10

-- We'll work with reverse digit arrays
-- so we check for non-increasing order

isNonIncr :: [Int] -> Bool
isNonIncr lst = all (\(a, b) -> a >= b) $ zip lst (tail lst)

hasDouble :: [Int] -> Bool
hasDouble lst = any (\(a, b) -> a == b) $ zip lst (tail lst) 

isGood :: Int -> Bool
isGood n = 
  let ds = digitsR n
  in isNonIncr ds && hasDouble ds
  
-- fromEnum turns False to 0 and True to 1
countPasswds :: Int -> Int -> Int
countPasswds from to = sum $ fmap (fromEnum . isGood) [from .. to]

-- Chunk digits of a 6-digit number into groups of 4
-- after putting sentinel non-digits on both sides
chunk64 :: Int -> [[Int]]
chunk64 = fmap (take 4) . take 5 . tails . bracket10
  where -- put (non-digit) 10 on both sides of the digit array as sentinels
    bracket10 n = 10 : digitsR n ++ [10]

-- Two equal digits surrounded by other digits (or sentinels)
isPureDouble (a:b:c:d:[]) = a /= b && b == c && c /= d
isPureDouble x = error $ show x ++ "is not a four-letter word!"

hasPureDoubles :: Int -> Bool
hasPureDoubles n = or $ fmap isPureDouble $ chunk64 n

countPasswds' :: Int -> Int -> Int
countPasswds' from to = sum $ fmap isGood' [from .. to]
  where isGood' n = fromEnum (isGood n && hasPureDoubles n)

main = do
  print $ countPasswds  264793 803935
  print $ countPasswds' 264793 803935
