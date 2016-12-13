module PairsBy where

data Map key value = Map [(key, value)]

--get key map принимает ключ и мап, и возвращает Nothing, если сопоставления этому ключу нет, Just value, если ключу сопоставлено
--значение value.
get :: Eq k => k -> Map k v -> Maybe v
get _ (Map []) = Nothing
get key (Map ((x, y):other))
  | key == x  = Just y
  | otherwise = get key $ Map other

--put (key, value) map добавляет в мап пару ключ-значение (key, value) и возвращает измененный мап.
put :: Eq k => (k, v) -> Map k v -> Map k v
put (x, y) (Map []) = Map [(x, y)]
put (x, y) (Map (element@(key, _):other))
  | key == x  = Map ((key, y):other)
  | otherwise = Map $ element : newelements where
    Map newelements = put (x, y) (Map other)

--remove key map принимает ключ и мап и удаляет данный ключ из мапа. Возвращает измененный мап.
remove :: Eq k => k -> Map k v -> Map k v
remove _ (Map []) = Map []
remove key (Map (element@(x, _):other))
  | key == x  = Map other
  | otherwise = Map $ element : newelements where
    Map newelements = remove key (Map other)

--keys map принимает мап и возвращает список его ключей
keys :: Map k v -> [k]
keys (Map []) = []
keys (Map ((x, _):es)) = x : keys (Map es)

--values map принимает мап и возвращает список его значений
values :: Map k v -> [v]
values (Map []) = []
values (Map ((_, y):es)) = y : values (Map es)

--pairsBy predicate map принимает предикат и возвращает список пар (ключ, значение) из map, которые удовлетворяют этому предикату,
--примененному к ключу.
pairsBy :: (k -> Bool) -> Map k v -> [(k, v)]
pairsBy _ (Map []) = []
pairsBy f (Map ((x, y):other))
  | f x       = (x, y) : tailanswer
  | otherwise = tailanswer
  where
    tailanswer = pairsBy f (Map other)
