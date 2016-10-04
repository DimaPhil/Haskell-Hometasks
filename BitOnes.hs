--Бойцова Анастасия, A3301

module BitOnes where

bitOnes :: Integer -> Int
bitOnes 0 = 0
bitOnes x
  | x < 0 = error "The number should be non-negative"
  | x > 0 = (if x `mod` 2 == 1 then 1 else 0) + bitOnes (x `div` 2)
