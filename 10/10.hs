import Data.List

middle :: [a] -> [a]
middle l@(_ : _ : _ : _) = middle $ tail $ init l
middle l = l

opening :: Char -> Bool
opening char = char `elem` ['(', '[', '{', '<']

closing :: Char -> Bool
closing char = char `elem` [')', ']', '}', '>']

match :: Char -> Char
match char = case char of
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'
  _ -> error ("Illegal character " ++ [char])

reduceLine :: String -> Either (Char, Char) [Char]
reduceLine = foldl reducer (Right [])
 where
  reducer (Left p) _ = Left p
  reducer (Right stack) char
    | opening char = Right (match char : stack)
    | closing char = case stack of
      [] -> Left ('\00', char)
      x : xs -> if x == char then Right xs else Left (x, char)
    | otherwise = error ("Illegal character " ++ [char])

scoreCorrupt :: String -> Int
scoreCorrupt line = case reduceLine line of
  Right _ -> 0
  Left (_, found) -> score found
 where
  score char = case char of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _ -> 0

scoreIncomplete :: String -> Int
scoreIncomplete line = case reduceLine line of
  Left _ -> 0
  Right stack -> foldl (\s c -> 5 * s + score c) 0 stack
 where
  score char = case char of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4
    _ -> 0

main :: IO ()
main = do
  content <- readFile "in10.txt"
  let ls = lines content
  print (sum $ map scoreCorrupt ls)
  print (head $ middle $ sort $ filter (/= 0) $ map scoreIncomplete ls)
