module Main where

import Data.List (foldl')

f1 :: Integral a => [a] -> a
f1 = snd . foldl' f (Nothing, 0)
  where
    f (Nothing, acc) curr = (Just curr, acc)
    f (Just n, acc) curr
      | curr > n = (Just curr, acc + 1)
      | otherwise = (Just curr, acc)

f2 :: Integral a => [a] -> a
f2 xs = f1 (sum <$> windows xs)
  where
    windows [] = []
    windows [_] = []
    windows [_, _] = []
    windows xs@(a:b:c:_) = [[a, b, c]] ++ windows (drop 1 xs)

main :: IO ()
main = do
  input <- fmap read . lines <$> readFile "input"
  print $ "Part one: " ++ show (f1 input)
  print $ "Part two: " ++ show (f2 input)
