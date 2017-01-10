--Бойцова Анастасия, A3401

module Undef where

import Data.Set

data Expr = Constant Integer
          | Variable String
          | Operator String [Expr]
data Progr = Skip
           | Assign String Expr
           | Sequence [Progr]
           | If Expr Progr Progr
           | While Expr Progr

findVars :: Expr -> Set String
findVars (Constant _) = empty
findVars (Variable name) = singleton name
findVars (Operator _ exprs) = unions $ Prelude.map findVars exprs

unite :: Ord a => (Set a, Set a) -> (Set a, Set a) -> (Set a, Set a)
unite (a, b) (c, d) = (union a c, union b d)

explore :: Progr -> (Set String, Set String)
explore Skip = (empty, empty)
explore (Assign var expr) = (singleton var, findVars expr)
explore (If e p1 p2) = unite (empty, findVars e) (na, nu)
  where
    (a1, u1) = explore p1
    (a2, u2) = explore p2
    na = intersection a1 a2
    nu = union (union u1 u2) (difference (union a1 a2) na)
explore (While e p) = (empty, union (findVars e) (snd $ explore p))
explore (Sequence progrs) = go progrs empty empty
  where
    go :: [Progr] -> Set String -> Set String -> (Set String, Set String)
    go [] assigned unassigned = (assigned, unassigned)
    go (p:ps) assigned unassigned = let (a, u) = explore p
                                        nassigned = union a assigned
                                    in go ps nassigned (union unassigned (difference u assigned))

undef :: Progr -> [String]
undef p = toList $ snd (explore p)

test1 :: Progr
test1 = Sequence
        [
          Assign "y" (Constant 1),
          Assign "x" (Operator "+" [Variable "x", Variable "y"])
        ]

test2 :: Progr
test2 = Sequence
        [
          If (Operator "<" [Variable "x", Constant 0]) (Assign "y" (Constant 1)) (Assign "x" (Constant 0)),
          Assign "z" (Variable "y"),
          Assign "y" (Operator "+" [Variable "z", Constant 1])
        ]
