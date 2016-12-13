--Бойцова Анастасия, A3401

module Components where

import           Data.Set (empty, insert, notMember)

type Graph = [[Int]]

components :: Graph -> [[Int]]
components graph = helper graph empty 0
  where
    helper g used v
        | v == n    = []
        | otherwise =
          if notMember v used then
              vs : helper g newUsed (v + 1)
          else
              helper g used (v + 1)
        where
          n = length graph
          (newUsed, vs) = dfs g used v
    dfs g used v = foldl (\(usedv, vs) u -> if notMember u usedv
                          then
                              let (newUsed, us) = dfs g (insert u usedv) u in (newUsed, vs ++ us)
                          else
                              (usedv, vs))
                          (insert v used, [v]) (g !! v)
