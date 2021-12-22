type Matrix = [[Int]]
type Position = (Int, Int)

findItems :: (a -> Bool) -> [a] -> [(Int, a)]
findItems predicate = filter (predicate . snd) . zip [0 ..]

parseInput :: String -> Matrix
parseInput = map parseLine . lines
 where
  parseLine = map (read . (: []))

get :: Matrix -> Position -> Int
get m (x, y) = m !! x !! y

mapWithIndex :: (Position -> Int -> Int) -> Matrix -> Matrix
mapWithIndex f m = [[f (x, y) (get m (x, y)) | y <- [0 .. length (head m) - 1]] | x <- [0 .. length m - 1]]

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
  directions = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]

energize :: Matrix -> [Position] -> Matrix
energize = foldl (\m p -> mapWithIndex (inc p) m)
 where
  inc p = \(x, y) n -> if (x, y) == p then n + 1 else n

propagate :: Matrix -> [Position] -> Matrix
propagate m d = case concatMap (neighbours m) flash of
  [] -> m'
  xs -> propagate nextm nextd
   where
    nextd = flash ++ d
    nexta = filter (`notElem` nextd) xs
    nextm = energize m' nexta
 where
  m' = mapWithIndex (\_ n -> if n >= 10 then 0 else n) m
  flash = filter (\p -> notElem p d && get m p >= 10) (positions m)

step :: Matrix -> Matrix
step m = propagate (energize m (positions m)) []

simulate :: Matrix -> [Matrix]
simulate = iterate step

flashes :: Matrix -> Int
flashes = length . filter (== 0) . concat

main :: IO ()
main = do
  content <- readFile "in11.txt"
  let m = parseInput content
  print (sum $ take 100 $ map flashes $ drop 1 $ simulate m)
  print (fst $ head $ findItems (\m -> flashes m == (length . concat) m) $ simulate m)
