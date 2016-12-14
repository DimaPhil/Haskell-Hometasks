--Бойцова Анастасия, A3401

module Wave where

import qualified Data.Map.Lazy as M (Map (), empty, fromList, insert, lookup)
import           Data.Maybe    (fromJust)
import qualified Data.Sequence as S (Seq (), ViewL (..), fromList, null,
                                     viewl, (|>))

type Graph = (Int, Int -> Int -> Bool)

infinity :: Int
infinity = maxBound :: Int

--Функция восстановления пути - принимает начальную и конечную вершины,
-- а также мап, в котором хранятся оптимальные предки для всех вершин
--Ответ - просто рекурсивно запустить функцию для (start, parent[finish]) и добавить в конец вершину finish
--Здесь путь возвращается перевернутым, поэтому finish добавляется в начало
restorePath :: Int -> Int -> M.Map Int Int -> [Int]
restorePath start finish parents
    | start == finish = [start]
    | otherwise       = finish : restorePath start (fromJust $ M.lookup finish parents) parents

wave :: Int -> Int -> Graph -> Maybe [Int]
wave start finish graph
    -- если пути нет, то есть расстояние до вершины - бесконечность, то Nothing, иначе восстанавливаем ответ
    | ansDistance == infinity = Nothing
    | otherwise               = Just $ reverse (restorePath start finish ansParents)
    where
        (ansDistance, ansParents) = bfs start finish graph

        --helper - вспомогательная функция для обхода графа в ширину
        --принимает конечную вершину, граф, текущие расстояния (мап), текущих предков (мап), текущую очередь
        helper :: Int -> Graph -> M.Map Int Int -> M.Map Int Int -> S.Seq Int -> (Int, M.Map Int Int)
        helper fs (n, edgeExists) distances parents q
            --если очередь пустая, то bfs завершен, возвращаем пару из посчитанных расстояний и массива предков)
            --иначе, обновляем расстояния и предков для соседей первой вершины из очереди, запускаемся рекурсивно
            | S.null q  = (fromJust (M.lookup fs distances), parents)
            | otherwise = helper fs g newDistances newParents newQ
            where
                --fv - первая вершина в очереди, rest - оставшиеся элементы очереди
                (fv S.:< rest) = S.viewl q
                getDistance ds v = fromJust (M.lookup v ds)
                --adjList v -- список [0/1, child, v, d[child]] для всех соседей вершины v
                --1, если d[child] улучшилось благодаря вершине v, 0, если нет
                --на самом деле, оставляем только те вершины из этого списка, до которых расстояние улучшилось
                --то есть adjList v -- список соседей v, до которых улучшилось расстояние (с некоторой доп. информацией)
                adjList v = filter (\e -> head e == 1)
                                   (map (\u -> if edgeExists v u &&
                                          getDistance distances u >
                                          getDistance distances v + 1
                                       then [1, u, v, getDistance distances v + 1]
                                       else [0, u, v, getDistance distances u])
                                    [0..n - 1])

                --updateCollection - функция, которая обновляет переданную ей коллекцию, применяй переданную фукнцию
                --используется для получения новых расстояний, предков, очереди
                updateCollection :: a -> [[Int]] -> ([Int] -> a -> a) -> a
                updateCollection c [] _     = c
                updateCollection c (e:es) f = updateCollection (f e c) es f

                --Чтобы получить новые расстояния - для всех соседей fv, до которых расстояние улучшилось, обновляем его в мапе
                newDistances = updateCollection distances (adjList fv) (\e collection ->
                                                                       M.insert (e !! 1) (e !! 3) collection)
                --Для предков то же самое - если расстояние улучшилось, обновляем и предка
                newParents = updateCollection parents (adjList fv) (\e collection ->
                                                                   M.insert (e !! 1) (e !! 2) collection)
                --Новая очередь - все вершины, до которых расстояние улучшилось, добавляем в конец очереди
                newQ = updateCollection rest (adjList fv) (\e collection -> collection S.|> (e !! 1))

        --изначальные расстояния - все бесконечности, кроме начальной вершины
        initialDistances :: Int -> Int -> M.Map Int Int
        initialDistances n s =
            let infMap = M.fromList [(x, infinity) | x <- [0..(n - 1)]] in
            M.insert s 0 infMap

        --bfs s f graph обходит граф в ширину, используя вспомогательную функцию helper
        bfs :: Int -> Int -> Graph -> (Int, M.Map Int Int)
        bfs s f (n, _) = helper f g (initialDistances n s) M.empty (S.fromList [s])

isEdge :: Int -> Int -> Bool
isEdge i j = (i `elem` [0..2] && j `elem` [0..2]) || (i `elem` [3..5] && j `elem` [3..5]) || (i == 2 && j == 3)

g :: Graph
g = (6, isEdge)

path :: Maybe [Int]
path = wave 1 5 g
