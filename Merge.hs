--Бойцова Анастасия, A3401

module Merge where

data Map key value = Map [(key, value)]

get :: Eq k => k -> Map k v -> Maybe v
get _ (Map []) = Nothing
get key (Map ((x, y):other))
  | key == x  = Just y
  | otherwise = get key $ Map other

put :: Eq k => (k, v) -> Map k v -> Map k v
put (x, y) (Map []) = Map [(x, y)]
put (x, y) (Map (element@(key, _):other))
  | key == x  = Map ((key, y):other)
  | otherwise = Map $ element : newelements where
    Map newelements = put (x, y) (Map other)

remove :: Eq k => k -> Map k v -> Map k v
remove _ (Map []) = Map []
remove key (Map (element@(x, _):other))
  | key == x  = Map other
  | otherwise = Map $ element : newelements where
    Map newelements = remove key (Map other)

keys :: Map k v -> [k]
keys (Map []) = []
keys (Map ((x, _):es)) = x : keys (Map es)

values :: Map k v -> [v]
values (Map []) = []
values (Map ((_, y):es)) = y : values (Map es)

pairsBy :: (k -> Bool) -> Map k v -> [(k, v)]
pairsBy _ (Map []) = []
pairsBy f (Map ((x, y):other))
  | f x       = (x, y) : tailanswer
  | otherwise = tailanswer
  where
    tailanswer = pairsBy f (Map other)

type BinHeap e = [BinTree e]
data BinTree e = BinTree e [BinTree e] deriving Show

order :: BinTree e -> Int
order (BinTree _ xs) = calcOrder $ length xs where
  calcOrder 0 = 0
  calcOrder 1 = 0
  calcOrder l = calcOrder (l `div` 2) + 1

merge :: Ord e => BinHeap e -> BinHeap e -> BinHeap e
merge [] h2 = h2
merge h1 [] = h1
merge h1 h2
  | order (head h1) <= order (head h2) = uniqueOrders (head h1 : merge (tail h1) h2)
  | order (head h1) > order (head h2)  = uniqueOrders (head h2 : merge h1 (tail h2))

uniqueOrders :: (Ord e) => BinHeap e -> BinHeap e
uniqueOrders [] = []
uniqueOrders [n] = [n]
uniqueOrders (n1:n2:other)
    | order n1 /= order n2 = n1:n2:other
    | otherwise            = uniqueOrders (mergeEqual n1 n2 : other)

mergeEqual :: (Ord e) => BinTree e -> BinTree e -> BinTree e
mergeEqual tree1@(BinTree value1 children1) tree2@(BinTree value2 _)
    | value1 >= value2 = BinTree value1 (children1 ++ [tree2])
    | otherwise        = mergeEqual tree2 tree1
