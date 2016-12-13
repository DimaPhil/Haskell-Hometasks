module SumNumbers where

import           Data.Char (isDigit)

sumNumbers :: String -> Integer
sumNumbers [] = 0
sumNumbers [x]
  | isDigit x = read [x]
  | otherwise = 0
sumNumbers (x:xs)
  | isDigit x             = sumNumbers fremaining + fvalue
  | otherwise             = sumNumbers xs
  where
    (fvalue, fremaining) = head (reads (x:xs) :: [(Integer, String)])
