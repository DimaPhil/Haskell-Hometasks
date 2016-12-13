--Бойцова Анастасия, A3401

module Maze where

import           Data.Maybe (fromJust, isJust)
import           Data.Set   (Set (), empty, insert, member)

maze :: [[Integer]] -> Maybe [(Int, Int)]
maze field = fst $ dfs field empty (0, 0)
  where
    n = length field
    m = length (head field)
    dfs :: [[Integer]] -> Set (Int, Int) -> (Int, Int) -> (Maybe [(Int, Int)], Set (Int, Int))
    dfs matrix used (x, y)
        | x >= n || y >= m || member (x, y) used = (Nothing, used)
        | x == n - 1 && y == m - 1               = (Just [(n - 1, m - 1)], used)
        | x < n - 1 &&
          element >= downElement &&
          isJust downCall                        = (Just ((x, y) : fromJust downCall), newDownUsed)
        | y < m - 1 &&
          element >= rightElement &&
          isJust rightCall                       = (Just ((x, y) : fromJust rightCall), newRightUsed)
        | otherwise                              = (Nothing, newRightUsed)
      where
        element = matrix !! x !! y
        downElement = matrix !! (x + 1) !! y
        rightElement = matrix !! x !! (y + 1)
        (downCall, newDownUsed) = dfs matrix (insert (x, y) used) (x + 1, y)
        (rightCall, newRightUsed) = dfs matrix newDownUsed (x, y + 1)
