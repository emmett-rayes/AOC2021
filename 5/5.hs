import Data.List
import Data.List.Split

size = 1000 :: Integer

data Line = Line
  { start :: (Integer, Integer)
  , end :: (Integer, Integer)
  }
  deriving (Show)

parseLine :: String -> Line
parseLine string = Line (x1, y1) (x2, y2)
 where
  [xy1, xy2] = splitOn "->" string
  [x1, y1, x2, y2] = map read $ splitOn "," (xy1 ++ "," ++ xy2)

parseInput :: String -> [Line]
parseInput string = map parseLine (lines string)

isHorizontal :: Line -> Bool
isHorizontal l = y1 == y2
 where
  (x1, y1) = start l
  (x2, y2) = end l

isDiagonal :: Line -> Bool
isDiagonal l = x2 - x1 == y2 - y1 || x2 - x1 == y1 - y2
 where
  (x1, y1) = start l
  (x2, y2) = end l

isVertical :: Line -> Bool
isVertical l = x1 == x2
 where
  (x1, y1) = start l
  (x2, y2) = end l

pointOnLine :: Line -> (Integer, Integer) -> Bool
pointOnLine l p =
  (x2 - x1) * (y - y1) == (y2 - y1) * (x - x1)
    && (minimum [x1, x2] <= x && x <= maximum [x1, x2])
    && (minimum [y1, y2] <= y && y <= maximum [y1, y2])
 where
  (x, y) = p
  (x1, y1) = start l
  (x2, y2) = end l

coverage :: [Line] -> [Int]
coverage ls = map counter grid
 where
  grid = [(x, y) | x <- [0 .. size - 1], y <- [0 .. size - 1]]
  counter = \p -> length $ filter id [pointOnLine l p | l <- ls]

-- showCoverage :: [Line] -> String
-- showCoverage ls = unlines $ map concat $ transpose $chunksOf (fromInteger size) $ map show $ coverage ls

overlap :: [Line] -> Int
overlap ls = length $ filter (> 1) (coverage ls)

main :: IO ()
main = do
  content <- readFile "in5.txt"
  let ls = parseInput content
  let straight = filter (\l -> isHorizontal l || isVertical l) ls
  let diagonal = filter isDiagonal ls
  print (overlap straight)
  print (overlap (straight ++ diagonal))
