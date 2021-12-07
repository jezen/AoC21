module Main where

import Control.Monad (forM_)
import Data.Array.IO
import Data.List.Split (splitOn)

type Coord = (Int, Int)
type LineSegment = (Coord, Coord)

rangePerpendicular :: LineSegment -> [Coord]
rangePerpendicular ((x1, y1), (x2, y2)) =
  [ (x,y) | x <- [(min x1 x2)..(max x1 x2)]
          , y <- [(min y1 y2)..(max y1 y2)] ]

rangeDiagonal :: LineSegment -> [Coord]
rangeDiagonal ((x1, y1), (x2, y2)) =
  let xs = [x1,(if x1 < x2 then x1 + 1 else x1 - 1)..x2]
      ys = [y1,(if y1 < y2 then y1 + 1 else y1 - 1)..y2]
   in zip xs ys

perpendicular :: LineSegment -> Bool
perpendicular ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

diagonal :: LineSegment -> Bool
diagonal ((x1, y1), (x2, y2)) = abs (x2 - x1) == abs (y2 - y1)

matrixBounds :: [LineSegment] -> (Int, Int)
matrixBounds segments =
  let xs = concatMap (\((x1, _), (x2, _)) -> [ x1, x2 ]) segments
      ys = concatMap (\((_, y1), (_, y2)) -> [ y1, y2 ]) segments
   in (maximum ys, maximum xs)

toLineSegment :: String -> LineSegment
toLineSegment s =
  let [ x1, y1, x2, y2 ] = concatMap (splitOn ",") . splitOn " -> " $ s
   in ((read x1, read y1), (read x2, read y2))

f1 :: [LineSegment] -> IO Int
f1 xs = do
  m <- newArray ((0, 0), matrixBounds xs) 0 :: IO (IOUArray (Int, Int) Int)
  let coords = concatMap rangePerpendicular (filter perpendicular xs)
  forM_ coords $ \(x, y) -> readArray m (y, x) >>= writeArray m (y, x) . (1 +)
  getElems m >>= pure . length . filter (>= 2)

f2 :: [LineSegment] -> IO Int
f2 xs = do
  m <- newArray ((0, 0), matrixBounds xs) 0 :: IO (IOUArray (Int, Int) Int)
  let coordsP = concatMap rangePerpendicular (filter perpendicular xs)
      coordsD = concatMap rangeDiagonal (filter diagonal xs)
      coords = coordsP <> coordsD
  forM_ coords $ \(x, y) -> readArray m (y, x) >>= writeArray m (y, x) . (1 +)
  getElems m >>= pure . length . filter (>= 2)


main :: IO ()
main = do
  input <- map toLineSegment . lines <$> readFile "input"
  result1 <- f1 input
  result2 <- f2 input
  print $ "Part one: " <> show result1
  print $ "Part two: " <> show result2
