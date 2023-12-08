import Data.Array
import Data.Set qualified as S

tabulate :: [String] -> Array Int (Array Int Char)
tabulate input = array rowSpan $ zip (range rowSpan) (map f input)
  where
    rowSpan = (1, length input)
    f line = array colSpan $ zip (range colSpan) line where colSpan = (1, length line)

partOne :: [String] -> Bool
partOne = foldr f False . tabulate

f :: Array Int Char -> Bool -> Bool
f line b1 = foldr g b1 line

g :: Char -> Bool -> Bool
g _ _ = True
