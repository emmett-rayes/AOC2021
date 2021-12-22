import Data.List

type Matrix = [[Int]]
type Position = (Int, Int)

parseLine :: String -> [Int]
parseLine = map (read . (: []))

parseInput :: String -> Matrix
parseInput = map parseLine . lines

get :: Matrix -> Position -> Int
get m (x, y) = m !! x !! y

positions :: Matrix -> [Position]
positions m = [(x, y) | x <- [0 .. length m - 1], y <- [0 .. length (head m) - 1]]

neighbours :: Matrix -> Position -> [Position]
neighbours m (x, y) =
  [ (x + dx, y + dy)
  | (dx, dy) <- directions
  , 0 <= x + dx && x + dx <= length m - 1
  , 0 <= y + dy && y + dy <= length (head m) - 1
  ]
 where
  directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]

lowPoints :: Matrix -> [Position]
lowPoints m = filter isLowPoint (positions m)
 where
  isLowPoint p = all ((v <) . get m) (neighbours m p) where v = get m p

basin :: Matrix -> Position -> [Position]
basin m p = nub $ p : concatMap (basin m) consecNeighbours
 where
  consecNeighbours = filter consecutive (neighbours m p)
  consecutive np = get m p < get m np && get m np < 9

main :: IO ()
main = do
  content <- readFile "in9.txt"
  let m = parseInput content
  print (sum $ map ((1 +) . get m) (lowPoints m))
  print (product $ take 3 $ reverse $ sort $ map (length . basin m) (lowPoints m))
