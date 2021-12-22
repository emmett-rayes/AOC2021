type BitVector = [Int]

parseLine :: String -> BitVector
parseLine line = map read (fmap pure line)

parseInput :: String -> [BitVector]
parseInput string = map parseLine (lines string)

bitVectorToInt :: BitVector -> Int
bitVectorToInt = foldl (\acc x -> 2 * acc + x) 0

invert :: BitVector -> BitVector
invert xs = [if x == 0 then 1 else 0 | x <- xs]

frequencies :: [BitVector] -> [Int]
frequencies report = foldl (zipWith (+)) (replicate (length (head report)) 0) report

mostCommonBitVector :: [BitVector] -> BitVector
mostCommonBitVector report =
  [if freq >= threshold then 1 else 0 | freq <- frequencies report]
 where
  len = length report
  threshold = (len `div` 2) + len `mod` 2

leastCommonBitVector :: [BitVector] -> BitVector
leastCommonBitVector = invert . mostCommonBitVector

gammaRate :: [BitVector] -> Int
gammaRate = bitVectorToInt . mostCommonBitVector

epsilonRate :: [BitVector] -> Int
epsilonRate = bitVectorToInt . leastCommonBitVector

rating :: ([BitVector] -> BitVector) -> Int -> [BitVector] -> BitVector
rating mcf n [x] = x
rating mcf n xs = rating mcf (n + 1) (filter (\x -> x !! n == mcf xs !! n) xs)

oxygenRating :: [BitVector] -> Int
oxygenRating report = bitVectorToInt (rating mostCommonBitVector 0 report)

co2Rating :: [BitVector] -> Int
co2Rating report = bitVectorToInt (rating leastCommonBitVector 0 report)

main :: IO ()
main = do
  content <- readFile "in3.txt"
  let report = parseInput content
  let gamma = gammaRate report
  let epsilon = epsilonRate report
  let oxygen = oxygenRating report
  let co2 = co2Rating report
  print (gamma * epsilon)
  print (oxygen * co2)
