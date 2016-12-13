--Бойцова Анастасия, A3401

module RootSin where

--rootSin x находит корень уравнения x + sin(x) = 2 методом дихотомии (так как это возврастающая функция).
rootSin :: Double -> Double
rootSin = search 0 5 where
  search l r acc
    | r - l <= acc   = (l + r) / 2
    | m + sin m <= 2 = search m r acc
    | otherwise      = search l m acc
    where
      m = (l + r) / 2
