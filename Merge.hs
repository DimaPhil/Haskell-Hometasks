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
data BinTree e = BinTree e [BinTree e]

merge :: Ord e => BinHeap e -> BinHeap e -> BinHeap e
merge = undefined
