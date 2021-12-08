module Main where

import Data.Map as M hiding (foldl, take)
import Data.Maybe (fromMaybe)

f :: Integral a => Map a a -> Map a a
f fish =
  let newPopulation = mapKeys pred fish
      newFish = fromMaybe 0 (M.lookup (-1) newPopulation)
   in delete (-1) (insert 8 newFish (adjust (newFish +) 6 newPopulation))

compute :: Int -> Map Int Int -> Int
compute n = sum . elems . last . take (n + 1) . iterate f

parse :: String -> Map Int Int
parse = g . read . (\s -> "[" <> s <> "]")
  where g = foldl (\m_ n -> adjust succ n m_) m :: [Int] -> Map Int Int
        m = (M.fromList (zip [0..8] (cycle [0]))) :: Map Int Int

main :: IO ()
main = do
  input <- parse <$> readFile "input"
  print $ "Part one: " ++ show (compute 80 input)
  print $ "Part two: " ++ show (compute 256 input)
