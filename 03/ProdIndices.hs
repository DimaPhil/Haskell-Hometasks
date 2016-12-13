module ProdIndices where

import           Data.List (zip4)

--prodIndices elements возвращает список индексов i массива, для которых a[i] == a[i - 1] * a[i + 1]. Если левого/правого соседа нет,
--он принимается равным единице.
prodIndices :: [Integer] -> [Int]
prodIndices elements = foldr (\(e, le, ne, i) lst -> if e == le * ne then i : lst else lst) [] zipped where
    zipped = zip4 elements (1 : elements) (tail elements ++ [1]) [0..]
