import Data.Char
import Data.List
import Data.List.Split

type Vertex = String
type Edge = (Vertex, Vertex)

parseLine :: String -> Edge
parseLine = toTuple . splitOn "-"
 where
  toTuple [x, y] = (x, y)
  toTuple _ = error "Parse Error"

parseInput :: String -> [Edge]
parseInput = map parseLine . lines

inverse :: [(a, b)] -> [(b, a)]
inverse = map (\(x, y) -> (y, x))

successors :: [Edge] -> Vertex -> [Vertex]
successors xs x = [y | (x', y) <- xs, x' == x]

isSmall :: Vertex -> Bool
isSmall = not . isUpper . head

paths :: [Edge] -> [Vertex] -> Vertex -> [[Vertex]]
paths g d v = append v $ concatMap (paths g nextd) nextv
 where
  append v xs = [v] : map (v :) xs
  nextd = d ++ [v | isSmall v, v /= "start", v /= "end"]
  nextv = if v == "end" then [] else filter (/= "start") $ f (successors g v)
  f = filter (`notElem` nextd)

paths2 :: [Edge] -> [Vertex] -> Vertex -> [[Vertex]]
paths2 g d v = append v $ concatMap (paths2 g nextd) nextv
 where
  append v xs = [v] : map (v :) xs
  nextd = d ++ [v | isSmall v, v /= "start", v /= "end"]
  nextv = if v == "end" then [] else filter (/= "start") $ f (successors g v)
  f = if length nextd > length (nub nextd) then filter (`notElem` nextd) else id

isInitial :: [Vertex] -> Bool
isInitial = (== "start") . head

isTerminal :: [Vertex] -> Bool
isTerminal = (== "end") . last

uniquePaths :: [Edge] -> [[Vertex]]
uniquePaths g = paths (g ++ inverse g) [] "start"

almostUniquePaths :: [Edge] -> [[Vertex]]
almostUniquePaths g = paths2 (g ++ inverse g) [] "start"

main :: IO ()
main = do
  content <- readFile "in12.txt"
  let g = parseInput content
  print (length $ filter isInitial $ filter isTerminal $ uniquePaths g)
  print (length $ filter isInitial $ filter isTerminal $ almostUniquePaths g)
