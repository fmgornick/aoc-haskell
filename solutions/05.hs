module Day05 where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)
import Text.Regex.Applicative (RE, some, string, sym, (=~))
import Text.Regex.Applicative.Common (decimal)

type Seed = Int
data SeedMap = SeedMap {_destination, _source, _range :: Int} deriving (Show, Eq, Ord)
data SeedData = SeedData {_seeds :: [Int], _maps :: [[SeedMap]]} deriving (Show)

seeds :: RE Char [Int]
seeds = string "seeds:" *> some (sym ' ' *> decimal)

seedmap :: RE Char SeedMap
seedmap = SeedMap <$> decimal <* sym ' ' <*> decimal <* sym ' ' <*> decimal

parse :: String -> SeedData
parse input = SeedData (fromJust (head h =~ seeds)) (map (mapMaybe (=~ seedmap)) t)
  where
    (h : t) = splitOn [""] (lines input)

next :: Seed -> [SeedMap] -> Seed
next seed seedMaps = if null valid then seed else seed + dest - src
  where
    valid = filter (\(SeedMap _ src rng) -> (seed >= src) && (seed < src + rng)) seedMaps
    SeedMap dest src _ = head valid

partOne :: SeedData -> Int
partOne (SeedData seeds []) = minimum seeds
partOne (SeedData seeds (h : t)) = partOne (SeedData (map (`next` h) seeds) t)

partTwo :: SeedData -> Int
partTwo (SeedData seeds seedMaps) = partOne (SeedData (newSeeds seeds) seedMaps)
  where
    newSeeds (seed : num : t) = [seed .. (seed + num)] ++ newSeeds t
