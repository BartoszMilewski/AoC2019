module Day3 where
import Data.List.Split
import Data.Maybe
import Data.List
import Control.Applicative

type Dir = Char
data Move = M Dir Int -- direction, distance
  deriving Show

readMv :: String -> Move
readMv (c:cs) = M c (read cs)

type Steps = Int -- number of steps in the grid
type Count = Int -- count of segments

-- A segment may be horizontal or vertical

data Seg = S { sMin :: Int  -- minimum coordinate
             , sMax :: Int  -- maximum coordinate
             , sOth :: Int  -- other coordinate
             , sDir :: Dir  -- direction
             , steps :: Steps -- grid steps so far
             } deriving Show

-- It works if their directions are orthogonal
    
isCross :: Seg -> Seg -> Bool
isCross s1 s2 = sMin s1 < sOth s2 
             && sOth s2 < sMax s1
             && sMin s2 < sOth s1
             && sOth s1 < sMax s2
             
type Pos = (Int, Int) -- x, y position

-- vertical and horizontal segments

data Segments = Segs { vert :: [Seg], hor :: [Seg] }
  deriving Show

addSeg :: Seg -> Segments -> Segments
addSeg seg (Segs vert hor) =
  case sDir seg of
    'U' -> Segs (seg : vert) hor
    'D' -> Segs (seg : vert) hor
    'L' -> Segs vert (seg : hor)
    'R' -> Segs vert (seg : hor)

-- start position, 
-- number of grid steps so far, 
-- cumulative vertical and horizontal segment lists
move :: (Pos, Steps, Segments) -> Move -> (Pos, Steps, Segments)
move ((x, y), steps, segs) (M dir dist) =
   let steps' = steps + dist
       seg' = S min max other dir steps
   in (pos', steps', addSeg seg' segs)
 where 
   (pos', min, max, other) =
     case dir of
      'U' -> ((x, y + dist), y, (y + dist), x)
      'D' -> ((x, y - dist), (y - dist), y, x)
      'L' -> ((x - dist, y), (x - dist), x, y)
      'R' -> ((x + dist, y), x, (x + dist), y)
    
-- In: vertical segment, horizontal segment
-- Out: if they cross, position and grid steps to crossing
cross :: (Seg, Seg) -> Maybe (Pos, Steps)
cross (vert, hor) =
    if isCross vert hor
    then Just ( (sOth vert, sOth hor)
              , steps vert + steps hor + dx + dy)
    else Nothing
  where dx = case sDir hor of
               'L' -> sMax hor - sOth vert
               'R' -> sOth vert - sMin hor
               _   -> error "Bad cross"
        dy = case sDir vert of
               'D' -> sMax vert - sOth hor
               'U' -> sOth hor - sMin vert
               _   -> error "Bad cross"
  
  
-- In: vertical segments, horizontal segments
-- Out: list of positions and steps to crossing
crosses :: [Seg] -> [Seg] -> [(Pos, Steps)]
crosses vs hs = 
  let pairs = (,) <$> vs <*> hs
  in catMaybes $ fmap cross pairs

manhDist :: Pos -> Int
manhDist (x, y) = abs x + abs y

main = do
  text <- readFile "Data3.txt"
  let ls = lines text
  let l1 = head ls
  let l2 = head (tail ls)
  --let l1 = test1
  --let l2 = test2
  let moves1 = fmap readMv $ splitOn "," l1
  let moves2 = fmap readMv $ splitOn "," l2
  
  let (_, _, segs1) = foldl move ((0, 0), 0, (Segs [] [])) moves1
  let (_, _, segs2) = foldl move ((0, 0), 0, (Segs [] [])) moves2
  -- we have lists of vertical/horizontal segments for each wire
  -- cross them all accordingly
  let cs = crosses (vert segs1) (hor segs2) 
        ++ crosses (vert segs2) (hor segs1)
        
  let mDists = fmap (manhDist . fst) cs
  print $ minimum mDists
  
  let wireDist = fmap snd cs
  print $ minimum wireDist
    
test1 = "R8,U5,L5,D3"
test2 = "U7,R6,D4,L4"
