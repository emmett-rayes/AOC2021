import Data.List.Extras
import Data.List.Split

parseInput :: Fractional a => String -> [a]
parseInput = map (fromIntegral . read) . splitOn ","

gauss :: Fractional a => a -> a
gauss x = x * (x + 1) / 2

distance :: Num a => [a] -> a -> a
distance xs y = sum [abs (x - y) | x <- xs]

nonLinearDistance :: Fractional a => [a] -> a -> a
nonLinearDistance xs y = sum [gauss $ abs (x - y) | x <- xs]

optimise :: (Ord a, Enum a, Ord b) => ([a] -> a -> b) -> [a] -> a
optimise f xs = argmin (f xs) [minimum xs .. maximum xs]

main :: IO ()
main = do
  content <- readFile "in7.txt"
  let crabs = parseInput content
  let distF = distance
  print (distF crabs $ optimise distF crabs)
  let distF = nonLinearDistance
  print (distF crabs $ optimise distF crabs)
