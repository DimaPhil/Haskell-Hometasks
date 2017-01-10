--Бойцова Анастасия, A3401

module ReduceEta where

import           Free (Expression (..), free)

reduceEta :: Expression -> Expression
--Constant, Variable и Function не нужно редуцировать
reduceEta (Constant x) = Constant x
reduceEta (Variable name) = Variable name
reduceEta (Function f) = Function f
--Чтобы редуцировать If, редуцируем каждую из компонент по отдельности
reduceEta (If p t e) = If (reduceEta p) (reduceEta t) (reduceEta e)
--Применение - то же самое
reduceEta (Application e e') = Application (reduceEta e) (reduceEta e')
--Если Lambda "x" (Application e (Variable x)), причем x не входит свободно в e, редуцируем, иначе - редуцируем только e
reduceEta (Lambda name e) = case e of
    Application e' (Variable name') -> if (name == name') && (name `notElem` free e')
                                       then reduceEta e'
                                       else Lambda name (Application (reduceEta e') (Variable name'))
    e'                              -> case reduceEta e' of
        Application _ (Variable _) -> reduceEta (Lambda name (reduceEta e'))
        _                          -> Lambda name (reduceEta e')
--Если Let, то редуцируем правые части и само выражение
reduceEta (Let xs e) = Let (map (\(x, y) -> (x, reduceEta y)) xs) (reduceEta e)

test1 :: Expression
test1 = Lambda "x" (Application (Lambda "y" (Lambda "x" (Application (Variable "y")
                                                                    (Application (Function "+")
                                                                                 (Variable "x")))))
                   (Variable "x"))

test2 :: Expression
test2 = Lambda "x" (Application (Variable "x") (Variable "x"))

test3 :: Expression
test3 = Lambda "x" (Application (Application (Function "+") (Lambda "y" (Application (Constant 2) (Variable "y")))) (Variable "x"))

test4 :: Expression
test4 = Application (Lambda "x" (Application (Application (Function "+") (Constant 2)) (Lambda "y" (Application (Variable "x") (Variable "y"))))) (Constant 3)
