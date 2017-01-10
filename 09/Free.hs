--Бойцова Анастасия, A3401

module Free where

import Data.Set

data Expression = Constant Integer
                | Variable String
                | Function String
                | If Expression Expression Expression
                | Application Expression Expression
                | Lambda String Expression
                | Let [(String, Expression)] Expression
                deriving (Show)

getFree :: Expression -> Set String
--Для Constant, Variable, Function - все понятно
getFree (Constant _) = empty
getFree (Variable name) = singleton name
getFree (Function _) = empty
--Для If - объединяем результаты запуска для трех выражений
getFree (If p t e) = union (getFree p) (union (getFree t) (getFree e))
--Применение - то же самое
getFree (Application e e') = union (getFree e) (getFree e')
--Лямбда - находим свободные переменные в выражении и убираем из них связанную по лямбде
getFree (Lambda name e) = difference (getFree e) (singleton name)
--Рекурсивный блок - находим свободные переменные в самом выражении и удаляем из этого множества все
--переменные слева в блоках Let, а затем объединяем со свободными переменными справа (игнорируя тоже те, что слева)
getFree (Let xs e) = union (difference (getFree e) leftfv) (difference rightfv leftfv) --union (getFree e) lets
  where
    leftfv = fromList $ Prelude.map fst xs
    rightfv = unions $ Prelude.map (getFree . snd) xs

    --xor :: Ord a => Set a -> Set a -> Set a
    --xor a b = union (difference a b) (difference b a)

    --lets = xor leftfv rightfv

free :: Expression -> [String]
free expression = toList $ getFree expression

test0 :: Expression
test0 = Lambda "x" (Lambda "y" (Application (Application (Function "+") (Variable "x")) (Variable "y")))

test1 :: Expression
test1 = Lambda "x" (Lambda "y" (Application (Function "-") (Variable "x")))

test2 :: Expression
test2 = Lambda "x" (Lambda "y" (Application (Function "-") (Variable "y")))

test3 :: Expression
test3 = Let [("plus", Lambda "x" (Lambda "y" (Application (Application (Function "+") (Variable "x")) (Variable "y"))))]
           (Application (Application (Variable "plus") (Constant 1)) (Constant 2))

test4 :: Expression
test4 = Variable "x"

test5 :: Expression
test5 = Lambda "x" (Lambda "y" (Application (Function "-") (Variable "z")))

test6 :: Expression
test6 = Let [("x", Constant 1)] (Variable "x")

test7 :: Expression
test7 = Let [("x", Constant 1), ("y", Application (Application (Function "+") (Variable "x")) (Constant 2))]
            (Variable "x")
