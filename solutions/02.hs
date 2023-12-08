module Day02 where

import Data.Char (isAlphaNum, isSpace)
import Data.List (maximumBy)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Ord (comparing)

maxNum :: M.Map String Int
maxNum = M.fromList [("red", 12), ("green", 13), ("blue", 14)]

dropSymbols :: [String] -> [String]
dropSymbols = map $ filter (\c -> isAlphaNum c || isSpace c)

parseLine :: [String] -> [[(Int, String)]]
parseLine = map (f . tail . tail . splitOn " ") . dropSymbols
  where
    f [] = []
    f (num : color : tail) = (read num :: Int, color) : f tail

linePower :: [(Int, String)] -> Int
linePower [] = 1
linePower list = max * linePower [x | x <- list, snd x /= color]
  where
    (max, color) = maximumBy (comparing fst) list

valid :: [String] -> Bool
valid [] = True
valid (num : color : t) = fromJust (M.lookup color maxNum) >= read num && valid t

partOne :: [String] -> Int
partOne = foldr (f . splitOn " ") 0 . dropSymbols
  where
    f :: [String] -> Int -> Int
    f (_ : id : tail) acc = if valid tail then acc + read id else acc

partTwo :: [String] -> Int
partTwo = sum . map linePower . parseLine
