parseInput :: String -> [Integer]
parseInput string = map read (lines string)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

triples :: [a] -> [(a, a, a)]
triples xs = zip3 xs (tail xs) (tail (tail xs))

countIncreasing :: Ord a => [a] -> Int
countIncreasing xs = length [y | (x, y) <- pairs xs, x < y]

countIncreasingWindow :: (Ord a, Num a) => [a] -> Int
countIncreasingWindow xs = countIncreasing [x + y + z | (x, y, z) <- triples xs]

main :: IO ()
main = do
  content <- readFile "in1.txt"
  let measurements = parseInput content
  let numIncreasing = countIncreasing measurements
  let numIncreasingWindow = countIncreasingWindow measurements
  print numIncreasing
  print numIncreasingWindow
