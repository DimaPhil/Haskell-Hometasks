--Бойцова Анастасия, A3301

module RootSin where

factorial :: Int -> Integer
factorial 0 = 1
factorial n = fromIntegral n * factorial (n - 1)

ch :: Double -> Int -> Double
ch _ 0 = 1
ch x n = ch x (n - 1) + x ^ (2 * n) / fromIntegral (factorial (2 * n))

rootSin :: Double -> Double
rootSin = search 0 5 where
  search l r acc
    | r - l <= acc   = (l + r) / 2
    | m + sin m <= 2 = search m r acc
    | otherwise      = search l m acc
    where
      m = (l + r) / 2
