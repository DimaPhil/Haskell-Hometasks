--Бойцова Анастасия, A3401

module BuildDiag where

buildDiag :: [a] -> [[a]]
buildDiag xs = [[xs !! ((i + j) * (i + j + 1) `div` 2 + i) | j <- [0..]] | i <- [0..]]
