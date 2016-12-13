--Бойцова Анастасия, A3401

module Increasing where

--increasing elements находит наибольший по длине подотрезок массива, в котором элементы неубывают. Если таких отрезков несколько,
--возвращает любой.
increasing :: Ord a => [a] -> [a]
increasing elements = take len $ drop start elements where
  find [] _ curans curindex maxans maxindex =
    if curans < maxans then (maxans, maxindex) else (curans, curindex - curans)
  find (x:xs) lastel curans curindex maxans maxindex
    | x >= lastel = find xs x (curans + 1) (curindex + 1) maxans maxindex
    | otherwise   = find xs x 1 (curindex + 1) newans newindex
    where
      newindex = if maxans < curans then curindex - curans else maxindex
      newans   = max maxans curans
  (len, start) = find (tail elements) (head elements) 1 1 1 1
