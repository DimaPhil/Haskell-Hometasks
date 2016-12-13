--Бойцова Анастасия, A3401

module Halves where

go :: Int -> [Integer] -> [Integer] -> [Integer] -> Integer -> Integer -> Int -> Maybe ([Integer], [Integer])
go n elements takenL takenR sumL sumR pos
    | pos == n  = if sumL == sumR then Just (takenL, takenR) else Nothing
    | otherwise = if (sumL + sum (drop pos elements) < sumR) ||
                     (sumR + sum (drop pos elements) < sumL) then
                      Nothing
                  else case go n elements (current : takenL) takenR (sumL + current) sumR (pos + 1) of
                      Nothing -> go n elements takenL (current : takenR) sumL (sumR + current) (pos + 1)
                      Just x  -> Just x
    where
        current = elements !! pos

halves :: [Integer] -> Maybe ([Integer], [Integer])
halves elements = go (length elements) elements [] [] 0 0 0
