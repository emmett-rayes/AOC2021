import Data.List.Split

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

step :: [Int] -> [Int]
step xs = map f xs ++ [8 | x <- xs, x == 0]
 where
  f 0 = 6
  f x = x - 1

model :: Int -> [Int] -> [Int]
model n xs = iterate step xs !! n

store :: (Int -> Int -> a) -> [[a]]
store f = map (\x -> map (f x) [0 ..]) [0 ..]

count :: Int -> Int -> Int
count _ 0 = 1
count 0 n = fastcount 6 (n - 1) + fastcount 8 (n - 1)
count x n = fastcount (x - 1) (n - 1)

countstore :: [[Int]]
countstore = store count

fastcount :: Int -> Int -> Int
fastcount x n = countstore !! x !! n

main :: IO ()
main = do
  content <- readFile "in6.txt"
  let initial = parseInput content
  print (sum $ map (`fastcount` 256) initial)
