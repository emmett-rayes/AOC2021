type Position = (Integer, Integer)

type PositionWithAim = (Integer, Integer, Integer)

parseLine :: String -> Position
parseLine line = case direction of
  "up" -> (0, - n)
  "down" -> (0, n)
  "forward" -> (n, 0)
  _ -> (0, 0)
 where
  [direction, strN] = words line; n = read strN

parseInput :: String -> [Position]
parseInput string = map parseLine (lines string)

addPosition :: Position -> Position -> Position
addPosition (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

computePosition :: [Position] -> Position
computePosition = foldl addPosition (0, 0)

addPositionWithAim :: PositionWithAim -> Position -> PositionWithAim
addPositionWithAim (x1, y1, z1) (x2, y2) = (x1 + x2, y1 + z1 * x2, z1 + y2)

computePositionWithAim :: [Position] -> PositionWithAim
computePositionWithAim = foldl addPositionWithAim (0, 0, 0)

main :: IO ()
main = do
  content <- readFile "in2.txt"
  let directions = parseInput content
  let (x1, y1) = computePosition directions
  let (x2, y2, z2) = computePositionWithAim directions
  print (x1 * y1)
  print (x2 * y2)
