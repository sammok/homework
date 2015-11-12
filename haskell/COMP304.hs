-- Name: Qianglong Mo
-- Date: 2015.11.13
-- Title: COMP304 Assignment1


-- 1. Variations on a search (15 marks 5 each)

-- (a) Write a function count returns the number of times that a given item occurs in a list
--     of items. For example, count 1 [1,2,1,2,1] should return 3, while count 1 [] should
--     return 0.
count :: Integer -> [Integer] -> Integer
count s list = case list of
   []       -> 0
   x:xs   
      | x == s    -> 1 + (count s xs)
      | otherwise -> count s xs  


-- (b) Write a function lastPos which returns the position of the last occurrence of a given in
-- a list of items, and 0 if the item does not occur in the list. For example, 
-- lastPos 1 [1,2,1,2,1] should return 5, while lastPos 3 [1,2,1] should return 0.
lastPos :: Integer -> [Integer] -> Integer
lastPos s list = calPos s list 0 0 
   where
      calPos :: Integer -> [Integer] -> Integer -> Integer -> Integer
      calPos s list index pos = case list of
         []  -> pos
         x:xs 
            | x == s    -> calPos s xs (index+1) (index+1)
            | otherwise -> calPos s xs (index+1) (pos)


-- (c) Write a function allPos which returns a list of all positions at which 
-- a given item occurs in a list of items, in the order that they occur. 
-- For example, allPos 1 [1,2,1,2,1] should return [1,3,5], 
-- while allPos 3 [1,2,1] should return [].
allPos :: Integer -> [Integer] -> [Integer]
allPos s list = calPos s list 0 []
   where 
      calPos :: Integer -> [Integer] -> Integer -> [Integer] -> [Integer]
      calPos s list index result = case list of 
         [] -> result
         x:xs
            | x == s    -> calPos s xs (index+1) (result ++ [index+1])
            | otherwise -> calPos s xs (index+1) result
