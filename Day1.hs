module Day1 where
  
readInt :: String -> Int
readInt s = read s

fuel m = floor ((fromIntegral m) / 3) - 2

fuelTot m = sum $ takeWhile (> 0) $ iterate fuel (fuel m)
  
main = do
  text <- readFile "Data1"
  let ms = fmap readInt $ lines text
  print $ sum $ fmap fuel ms
  print $ sum $ fmap fuelTot ms