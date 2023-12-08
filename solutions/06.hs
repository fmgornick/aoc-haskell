module Day06 where

import Data.Maybe (mapMaybe)
import Text.Regex.Applicative (RE, some, string, sym, (<|>), (=~))
import Text.Regex.Applicative.Common (decimal)

line :: RE Char [Int]
line = (string "Time:" <|> string "Distance:") *> some (some (sym ' ') *> decimal)

parse :: String -> [(Int, Int)]
parse input = zip times distances
  where
    (times : distances : _) = mapMaybe (=~ line) (lines input)

parse2 :: String -> (Int, Int)
parse2 input = (times, distances)
  where
    (times : distances : _) = map (read . concatMap show) (mapMaybe (=~ line) (lines input))

partOne :: [(Int, Int)] -> Int
partOne = foldr f 1
  where
    f (time, dist) acc = (floor r1 - floor r2) * acc
      where
        (r1, r2) = ((t + sqrt (t ^ 2 - 4 * d)) / 2, (t - sqrt (t ^ 2 - 4 * d)) / 2)
        (t, d) = (fromIntegral time, fromIntegral dist)

partTwo :: (Int, Int) -> Int
partTwo = partOne . (: [])
