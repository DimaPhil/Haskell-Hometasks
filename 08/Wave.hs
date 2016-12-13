--Бойцова Анастасия, A3401

module Wave where

import qualified Data.Map.Lazy as M (Map (), empty, fromList, insert, lookup)
import           Data.Maybe    (fromJust)
import qualified Data.Sequence as S (Seq (), ViewL (..), fromList, null,
                                     viewl, (|>))

type Graph = (Int, Int -> Int -> Bool)

infinity :: Int
infinity = maxBound :: Int

restorePath :: Int -> Int -> M.Map Int Int -> [Int]
restorePath start finish parents
    | start == finish = [start]
    | otherwise       = finish : restorePath start (fromJust $ M.lookup finish parents) parents

wave :: Int -> Int -> Graph -> Maybe [Int]
wave start finish graph
    | ansDistance == infinity = Nothing
    | otherwise               = Just $ reverse (restorePath start finish ansParents)
    where
        (ansDistance, ansParents) = bfs start finish graph
        helper :: Int -> Graph -> M.Map Int Int -> M.Map Int Int -> S.Seq Int -> (Int, M.Map Int Int)
        helper fs (n, edgeExists) distances parents q
            | S.null q  = (fromJust (M.lookup fs distances), parents)
            | otherwise = helper fs g newDistances newParents newQ
            where
                (fv S.:< rest) = S.viewl q
                getDistance ds v = fromJust (M.lookup v ds)
                adjList v = filter (\e -> head e == 1)
                                   (map (\u -> if edgeExists v u &&
                                          getDistance distances u >
                                          getDistance distances v + 1
                                       then [1, u, v, getDistance distances v + 1]
                                       else [0, u, v, getDistance distances u])
                                    [0..n - 1])
                updateCollection :: a -> [[Int]] -> ([Int] -> a -> a) -> a
                updateCollection c [] _     = c
                updateCollection c (e:es) f = updateCollection (f e c) es f

                newDistances = updateCollection distances (adjList fv) (\e collection ->
                                                                       M.insert (e !! 1) (e !! 3) collection)
                newParents = updateCollection parents (adjList fv) (\e collection ->
                                                                   M.insert (e !! 1) (e !! 2) collection)
                newQ = updateCollection rest (adjList fv) (\e collection -> collection S.|> (e !! 1))

        initialDistances :: Int -> Int -> M.Map Int Int
        initialDistances n s =
            let infMap = M.fromList [(x, infinity) | x <- [0..(n - 1)]] in
            M.insert s 0 infMap

        bfs :: Int -> Int -> Graph -> (Int, M.Map Int Int)
        bfs s f (n, _) = helper f g (initialDistances n s) M.empty (S.fromList [s])

isEdge :: Int -> Int -> Bool
isEdge i j = j - i == 1 || i - j == 3

g :: Graph
g = (4, isEdge)

path :: Maybe [Int]
path = wave 1 3 g
