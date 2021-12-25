import Control.Arrow (Arrow (first, second))
import Data.Char (toLower)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.List.Utils (replace)

type Point = (Int, Int)

data Fold = X Int | Y Int deriving (Show)

parsePoint :: String -> Point
parsePoint = toTuple . map read . splitOn ","
 where
  toTuple [x, y] = (x, y)
  toTuple _ = error "Parse Error"

parseFold :: String -> Fold
parseFold = construct . splitOn "=" . replace "fold along " "" . map toLower
 where
  construct ["x", n] = X (read n)
  construct ["y", n] = Y (read n)
  construct _ = error "Parse Error"

parseInput :: String -> ([Point], [Fold])
parseInput string = (map parsePoint points, map parseFold folds)
 where
  [points, folds] = map lines $ splitOn "\n\n" string

applyFold :: Fold -> [Point] -> [Point]
applyFold f = nub . map folder
 where
  folder = case f of
    X n -> first (fold n)
    Y n -> second (fold n)
  fold n m = if m > n then 2 * n - m else m

printPoints :: [Point] -> String
printPoints points =
  unlines
    [ [if (x, y) `elem` points then '#' else '.' | x <- [0 .. maxx]]
    | y <- [0 .. maxy]
    ]
 where
  maxx = maximum $ map fst points
  maxy = maximum $ map snd points

main :: IO ()
main = do
  content <- readFile "in13.txt"
  let (points, folds) = parseInput content
  print (length $ foldl (flip applyFold) points [head folds])
  print (printPoints $ foldl (flip applyFold) points folds)
