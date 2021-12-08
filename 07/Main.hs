module Main where

f1 :: [Integer] -> Integer
f1 xs = minimum $ map f [(minimum xs)..(maximum xs)]
  where f n = sum (map (\p -> abs (p - n)) xs)

f2 :: [Integer] -> Integer
f2 xs = minimum $ map f [(minimum xs)..(maximum xs)]
  where f n = sum (map (\p -> triangular (abs (p - n))) xs)
        triangular x = x * (x + 1) `div` 2

main :: IO ()
main = do
  input <- read . (\s -> "[" <> s <> "]") <$> readFile "input"
  print $ "Part one: " ++ show (f1 input)
  print $ "Part two: " ++ show (f2 input)
