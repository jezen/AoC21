module Main where

import Data.List
import Data.Ord

-- https://stackoverflow.com/a/48438340/704015
bintodec :: Integral i => i -> i
bintodec 0 = 0
bintodec i = 2 * bintodec (div i 10) + (mod i 10)

oxygenGeneratorRating :: [String] -> Int -> String
oxygenGeneratorRating [ ] _ = error "Could not find oxygen generator rating."
oxygenGeneratorRating [a] _ = a
oxygenGeneratorRating xs  i =
  let bitGroups = group . sort . (\a -> a !! i) . transpose $ xs
      bitCriteria =
        case bitGroups of
          [a] -> head a
          ys  -> case compare (length (bitGroups !! 0)) (length (bitGroups !! 1)) of
            EQ -> '1'
            LT -> '1'
            GT -> '0'
   in oxygenGeneratorRating (filter (\a -> a !! i == bitCriteria) xs) (i + 1)

co2ScrubberRating :: [String] -> Int -> String
co2ScrubberRating [ ] _ = error "Could not find CO2 scrubber rating."
co2ScrubberRating [a] _ = a
co2ScrubberRating xs  i =
  let bitGroups = group . sort . (\a -> a !! i) . transpose $ xs
      bitCriteria =
        case bitGroups of
          [a] -> head a
          ys  -> case compare (length (bitGroups !! 0)) (length (bitGroups !! 1)) of
            EQ -> '0'
            LT -> '0'
            GT -> '1'
   in co2ScrubberRating (filter (\a -> a !! i == bitCriteria) xs) (i + 1)

f1 :: [String] -> Int
f1 xs = gamma xs * epsilon xs
  where
    common f    = head . head . sortBy f . group . sort
    mostCommon  = common (comparing (negate . length))
    leastCommon = common (comparing length)
    compute f   = bintodec . read . map f . transpose
    gamma       = compute mostCommon
    epsilon     = compute leastCommon

f2 :: [String] -> Int
f2 xs = o2Rating * co2Rating
  where o2Rating = bintodec . read $ oxygenGeneratorRating xs 0
        co2Rating = bintodec . read $ co2ScrubberRating xs 0

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  print $ "Part one: " ++ show (f1 input)
  print $ "Part two: " ++ show (f2 input)
