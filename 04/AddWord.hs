--Бойцова Анастасия, A3401

module AddWord where

import           Data.List (findIndex)

data Trie = Empty | Node Char [Trie] deriving Show
type Dictionary = [Trie]

--addWord str dictionary добавляет в словарь dictionary строку str и возвращает новый словарь.
addWord :: String -> Dictionary -> Dictionary
addWord [] []                     = [Empty]
addWord [] dict                   = dict
addWord (c:s) dict = let
    predicate Empty = False
    predicate (Node c' _) = c == c'
    in case findIndex predicate dict of
    Nothing -> Node c (addWord s []) : dict
    Just index -> xs ++ updateNode node : ys where
        (xs, node:ys) = splitAt index dict
        updateNode Empty = Empty
        updateNode (Node c' son) = case compare c c' of
            EQ -> Node c (addWord s son)
            _  -> Node c' son
