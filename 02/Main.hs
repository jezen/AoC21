module Main where

import Data.List (foldl')

data Move = Move Direction Int

instance Show Move where
  show (Move direction n) = show direction <> " " <> show n

instance Read Move where
  readsPrec _ s = case words s of
    ["forward", n] -> [(Move Forward (read n :: Int), "")]
    ["down", n]    -> [(Move Down (read n :: Int), "")]
    ["up", n]      -> [(Move Up (read n :: Int), "")]
    _              -> error $ "Could not parse move: " <> s

data Direction = Forward | Down | Up

instance Show Direction where
  show Forward = "forward"
  show Down    = "down"
  show Up      = "up"

instance Read Direction where
  readsPrec _ "forward" = [(Forward, "")]
  readsPrec _ "down"    = [(Down, "")]
  readsPrec _ "up"      = [(Up, "")]
  readsPrec _ d         = error $ "Could not parse direction: " <> d

f1 :: [Move] -> Int
f1 moves = x * y
  where
    (x, y) = foldl' move (0,0) moves
    move (x, y) (Move direction n) = case direction of
      Forward -> (x + n, y)
      Down    -> (x, y + n)
      Up      -> (x, y - n)

f2 :: [Move] -> Int
f2 moves = x * y
  where
    ((x, y), aim) = foldl' move ((0, 0), 0) moves
    move ((x, y), aim) (Move direction n) = case direction of
      Forward -> ((x + n, y + (aim * n)), aim)
      Down    -> ((x, y), aim + n)
      Up      -> ((x, y), aim - n)

main :: IO ()
main = do
  input <- fmap read . lines <$> readFile "input"
  print $ "Part one: " ++ show (f1 input)
  print $ "Part two: " ++ show (f2 input)
