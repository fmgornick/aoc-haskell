module Day01 where

import Data.Bifunctor (first)
import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)

digits =
    [ ("one", '1')
    , ("two", '2')
    , ("three", '3')
    , ("four", '4')
    , ("five", '5')
    , ("six", '6')
    , ("seven", '7')
    , ("eight", '8')
    , ("nine", '9')
    ]
stigid = map (first reverse) digits

{-------------- HELPERS --------------}
checkForDigit :: [(String, Char)] -> String -> Maybe Char
checkForDigit [] _ = Nothing
checkForDigit ((word, chr) : t) line =
    if chr == head line || word `isPrefixOf` line
        then Just chr
        else checkForDigit t line

getDigit :: [(String, Char)] -> String -> Char
getDigit dig line = case checkForDigit dig line of
    Just c -> c
    Nothing -> getDigit dig (tail line)

{-------------------------------------}

partOne :: [String] -> Int
partOne = sum . map (((+) <$> ((* 10) . head) <*> last) . map digitToInt . filter isDigit)

partTwo :: [String] -> Int
partTwo = foldr (\line acc -> acc + read [getDigit digits line, getDigit stigid (reverse line)]) 0
