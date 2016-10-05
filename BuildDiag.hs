--Бойцова Анастасия, A3401

module BuildDiag where

import           Data.List

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge a@(x:xs) b@(y:ys)
  | x <= y    = x : merge xs b
  | otherwise = y : merge a ys

sqFact :: [Integer]
sqFact = nub $ merge [x * x - 1 | x <- [2..]] [factorial x | x <- [1..]]

buildDiag :: [a] -> [[a]]
buildDiag = undefined
