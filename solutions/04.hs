module Day04 where

import Data.List (intersect)
import Text.Regex.Applicative (RE, some, string, sym, (=~))
import Text.Regex.Applicative.Common (decimal)

data Numbered a = Numbered Int a deriving (Functor, Foldable, Show, Eq, Ord)
data Card = Card {_winners, _got :: [Int]} deriving (Show)

card :: RE Char Card
card = Card <$> some num <* string " |" <*> some num
  where
    num = some (sym ' ') *> decimal

line :: RE Char (Numbered Card)
line = Numbered <$> (string "Card" *> some (sym ' ') *> decimal <* sym ':') <*> card

partOne :: [Numbered Card] -> Int
partOne = foldr f 0
  where
    f :: Numbered Card -> Int -> Int
    f (Numbered _ (Card win got)) acc = if matches == 0 then acc else acc + 2 ^ (matches - 1)
      where
        matches = length (win `intersect` got)
