module Main where

import Data.List
import Data.List.Split (chunksOf, splitOn)

type Board = [Integer]

bingo :: [Integer] -> Board -> Bool
bingo numbers board =
  let rows = chunksOf 5 board
      cols = transpose rows
   in any (all (`elem` numbers)) rows || any (all (`elem` numbers)) cols

firstWinner :: [Board] -> [Integer] -> [Integer] -> ([Integer], Board)
firstWinner boards checked (checking:unchecked) =
  case filter (bingo checked) boards of
    (h:_) -> (checked, h)
    _     -> firstWinner boards (checked ++ [checking]) unchecked

lastWinner :: [Board] -> [Integer] -> ([Integer], Board)
lastWinner boards checked =
  let winners = filter (bingo checked) boards
      checking = filter (bingo (init checked)) boards
   in if winners /= checking
      then (checked, head $ winners \\ checking)
      else lastWinner boards (init checked)

f1 :: [Board] -> [Integer] -> Integer
f1 boards numbers = sum unmarked * last checked
  where (checked, winner) = firstWinner boards [] numbers
        unmarked = filter (`notElem` checked) winner

f2 :: [Board] -> [Integer] -> Integer
f2 boards numbers = sum unmarked * last checked
  where (checked, winner) = lastWinner boards numbers
        unmarked = filter (`notElem` checked) winner

main :: IO ()
main = do
  (numbers, boards) <- do
    input <- readFile "input"
    let ns = map read . splitOn "," . head . take 1 . lines
        bs = chunksOf 25 . map read . concatMap words . drop 2 . lines
    pure (ns input, bs input)
  print $ "Part one: " ++ show (f1 boards numbers)
  print $ "Part two: " ++ show (f2 boards numbers)
