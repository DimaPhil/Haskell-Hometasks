--Бойцова Анастасия, A3401

module SqFact where

import           Data.List (nub)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge a@(x:xs) b@(y:ys)
  | x <= y    = x : merge xs b
  | otherwise = y : merge a ys

sqFact :: [Integer]
sqFact = nub $ merge [x * x - 1 | x <- [2..]] (scanl (*) 1 [2..])
