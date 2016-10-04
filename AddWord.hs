--Бойцова Анастасия, A3301

module AddWord where

prodIndices :: [Integer] -> [Int]
prodIndices [] = []
prodIndices elements = process elements 0 1 where
  process [x] index lst = [index | lst == x]
  process xs index lst
    | x == lst * y = index : leftxs
    | otherwise  = leftxs
    where
      x = head xs
      y = xs !! 1
      leftxs = process (tail xs) (index + 1) x

data Trie = Empty | Node Char [Trie] deriving Show
type Dictionary = [Trie]
addWord :: String -> Dictionary -> Dictionary
addWord [] (node@(Node _ _):dict) = node : dict
addWord [] dict@(Empty:_)         = dict
addWord [] []                     = [Empty]
addWord (c:s) []                  = [Node c $ addWord s []]
addWord str (Empty:dict)          = Empty : addWord str dict
addWord str@(c:s) (node@(Node c' son):dict)
  | c == c'   = Node c (addWord s son) : dict
  | otherwise = node : addWord str dict
