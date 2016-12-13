--Бойцова Анастасия, A3401

module Merge where

type BinHeap e = [BinTree e]
data BinTree e = BinTree e [BinTree e] deriving Show

--order tree принимает биномиальное дерево и возвращает такое максимальное k, что 2^k < length(tree.children).
order :: BinTree e -> Int
order (BinTree _ xs) = calcOrder $ length xs where
  calcOrder 0 = 0
  calcOrder 1 = 0
  calcOrder l = calcOrder (l `div` 2) + 1

--merge heap1 heap2 принимает две биномиальных кучи и сливает их, возвращает новую биномиальную кучу.
merge :: Ord e => BinHeap e -> BinHeap e -> BinHeap e
merge [] h2 = h2
merge h1 [] = h1
merge h1 h2
  | order (head h1) <= order (head h2) = uniqueOrders (head h1 : merge (tail h1) h2)
  | order (head h1) > order (head h2)  = uniqueOrders (head h2 : merge h1 (tail h2))

--uniqueOrders heap убирает из кучи деревья с одинаковыми order (сливает их). Возвращает новую кучу-ответ.
uniqueOrders :: (Ord e) => BinHeap e -> BinHeap e
uniqueOrders [] = []
uniqueOrders [n] = [n]
uniqueOrders (n1:n2:other)
    | order n1 /= order n2 = n1:n2:other
    | otherwise            = uniqueOrders (mergeEqual n1 n2 : other)

--mergeEqual t1 t2 сливает два биномиальных дерева с одинаковыми порядками и возвращает новое дерево - результат.
mergeEqual :: (Ord e) => BinTree e -> BinTree e -> BinTree e
mergeEqual tree1@(BinTree value1 children1) tree2@(BinTree value2 _)
    | value1 <= value2 = BinTree value1 (children1 ++ [tree2])
    | otherwise        = mergeEqual tree2 tree1

--merge [(BinTree 13 []), (BinTree 1 [(BinTree 2 [(BinTree 11 [])]), (BinTree 3 [])])] [(BinTree 0 [(BinTree 11 [(BinTree 12 [])]), (BinTree 6 [])])]