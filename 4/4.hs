{-# LANGUAGE LambdaCase #-}

import Data.List
import Data.List.Split

size = 5 :: Int

type Board = [(Integer, Bool)]

foldlWhile :: (a1 -> a2 -> a1) -> a1 -> (a1 -> Bool) -> [a2] -> a1
foldlWhile f z p xs = head $ dropWhile p $ scanl f z xs

dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = take (n - 1) xs ++ dropEvery n (drop n xs)

parseRandNumbers :: String -> [Integer]
parseRandNumbers = map read . splitOn ","

parseBoard :: String -> Board
parseBoard = map (lift . read) . words
 where
  lift x = (x, False)

resetNumber :: Bool -> Integer -> Board -> Board
resetNumber t x xs = if found then head s ++ [(x, t)] ++ s !! 1 else xs
 where
  found = length s == 2
  s = splitWhen (\case (y, _) -> y == x) xs

markNumber :: Integer -> Board -> Board
markNumber = resetNumber True

unmarkNumber :: Integer -> Board -> Board
unmarkNumber = resetNumber False

isWinningBoard :: Board -> Bool
isWinningBoard board = winning rows || winning columns
 where
  rows = chunksOf size board
  columns = transpose rows
  winning xs = any (all snd) xs

boardScore :: Integer -> Board -> Integer
boardScore r board = r * score board
 where
  score board = sum $ map fst $ filter (not . snd) board

bingo :: [Integer] -> [Board] -> Integer
bingo rands boards = boardScore r winning
 where
  termCond = not . any isWinningBoard . snd
  firstRound = (head rands, boards)
  lastRound = foldlWhile reducer firstRound termCond rands
  reducer (_, boards) r = (r, map (markNumber r) boards)
  r = fst lastRound
  winning = head $ filter isWinningBoard (snd lastRound)

bingoLast :: [Integer] -> [Board] -> Integer
bingoLast rands boards = boardScore r (markNumber r losing)
 where
  termCond = not . all isWinningBoard . snd
  firstRound = (head rands, boards)
  lastRound = foldlWhile reducer firstRound termCond rands
  reducer (_, boards) r = (r, map (markNumber r) boards)
  r = fst lastRound
  losing = head $ filter (not . isWinningBoard) $ map (unmarkNumber r) (snd lastRound)

main :: IO ()
main = do
  content <- readFile "in4.txt"
  let ls = lines content
  let randstr = head ls
  let boardstr = dropEvery (size + 1) (drop 2 ls)
  let rands = parseRandNumbers randstr
  let boards = map (parseBoard . concat) $ chunksOf size (map (++ " ") boardstr)
  print (bingo rands boards)
  print (bingoLast rands boards)
