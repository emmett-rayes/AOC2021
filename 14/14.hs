import Control.Arrow
import Data.List
import Data.List.Split

type Insertion = (Char, Char, Char)

parseInsertion :: String -> Insertion
parseInsertion string = (a, b, c)
 where
  [[a, b], [c]] = splitOn " -> " string

parseInput :: String -> ([Char], [Insertion])
parseInput string = (template, map parseInsertion $ lines insertions)
 where
  [template, insertions] = splitOn "\n\n" string

frequencies :: (Eq a, Ord a) => [a] -> [(a, Int)]
frequencies = map (head &&& length) . group . sort

ochunk :: Int -> [a] -> [[a]]
ochunk n xs
  | length chunk' < n = []
  | otherwise = chunk' : ochunk n (tail xs)
 where
  chunk' = take n xs

merge :: [[a]] -> [a]
merge xs = concatMap init (init xs) ++ last xs

applyInsertion :: [Char] -> Insertion -> [Char]
applyInsertion p (a, b, c) = case p of
  [a', b'] | a == a' && b == b' -> [a, c, b]
  _ -> p

step :: [Insertion] -> [Char] -> [Char]
step insertions = merge . map (flip (foldl applyInsertion) insertions) . ochunk 2

{-
simulate :: Int -> [Char] -> [Insertion] -> [Char]
simulate n template insertions = merge $ iterate step (ochunk 2 template) !! n
 where
  step = concatMap (ochunk 2 . flip (foldl applyInsertion) insertions)
-}

main :: IO ()
main = do
  content <- readFile "in14.txt"
  let (template, insertions) = parseInput content
  let result = iterate (step insertions) template !! 10
  print result
