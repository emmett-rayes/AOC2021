{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

import Data.List
import Data.List.Split
import Data.Maybe

type Pair = ([String], [String])

data Matching = Matching {domain :: String, mapping :: Char -> Char}

instance Show Matching where
  show m = unwords [[x] ++ "->" ++ [mapping m x] | x <- domain m]

parseLine :: String -> Pair
parseLine line = (input, output)
 where
  [input, output] = map words $ splitOn "|" line

parseInput :: String -> [Pair]
parseInput string = map parseLine $ lines string

guess :: String -> [Int]
guess s = case length s of
  2 -> [1]
  3 -> [7]
  4 -> [4]
  5 -> [2, 3, 5]
  6 -> [0, 6, 9]
  7 -> [8]
  _ -> []

segments :: Int -> String
segments n = case n of
  0 -> "abcefg"
  1 -> "cf"
  2 -> "acdeg"
  3 -> "acdfg"
  4 -> "bcdf"
  5 -> "abdfg"
  6 -> "abdefg"
  7 -> "acf"
  8 -> "abcdefg"
  9 -> "abcdfg"

digits :: String -> Int
digits s = case sort s of
  "abcefg" -> 0
  "cf" -> 1
  "acdeg" -> 2
  "acdfg" -> 3
  "bcdf" -> 4
  "abdfg" -> 5
  "abdefg" -> 6
  "acf" -> 7
  "abcdefg" -> 8
  "abcdfg" -> 9

matchings :: String -> [Matching]
matchings s = [match p (segments n) | n <- guess s, p <- permutations s]
 where
  match s1 s2 =
    Matching
      { domain = s1
      , mapping = m
      }
   where
    m c = s2 !! fromJust (elemIndex c s1)

compatible :: Matching -> Matching -> Bool
compatible m1 m2 = and [mapping m1 x1 == mapping m2 x2 | x1 <- domain m1, x2 <- domain m2, x1 == x2]

combine :: Matching -> Matching -> Matching
combine m1 m2 =
  Matching
    { domain = nub (domain m1 ++ domain m2)
    , mapping = m
    }
 where
  m c
    | c `elem` domain m2 = mapping m2 c
    | c `elem` domain m1 = mapping m1 c

step :: String -> Matching -> [Matching]
step s m = map (combine m) $ filter (compatible m) (matchings s)

searchMatchings :: [String] -> [Matching]
searchMatchings = foldl f [empty]
 where
  empty = Matching{domain = ""}
  f ms s = concatMap (step s) ms

decode :: Matching -> String -> Int
decode m s = digits $ map (mapping m) s

solve :: [String] -> [String] -> Int
solve inputs outputs = read $ concat [show $ decode matching out | out <- outputs]
 where
  matching = head $ searchMatchings inputs

numUnique :: [Pair] -> Int
numUnique pairs = sum [trivial output | (input, output) <- pairs]
 where
  trivial output = length $ filter (\d -> head (guess d) `elem` [1, 4, 7, 8]) output

main :: IO ()
main = do
  content <- readFile "in8.txt"
  let pairs = parseInput content
  print (numUnique pairs)
  print (sum $ map (uncurry solve) pairs)
