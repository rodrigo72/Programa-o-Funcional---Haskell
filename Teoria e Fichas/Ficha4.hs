-- Resolução da ficha 4 de Programação Funcional 
module Ficha4 where

-- 9. a) 
-- [1,2,4,8,16,32,64,128,256,512,1024]

q9a :: [Integer] 
q9a = take 10 [2^x | x <- [1..]]
-- [2^x | x <- [1..], x <= 10] não resulta 

q9b :: [(Integer, Integer)]
q9b = [(x+1,y) | x <- [0,1,2,3,4], y <- [5-x]]
-- q9b  = [(x,y) | x <- [1,2,3,4,5], y <- [5,4,3,2,1]] não resulta

q9c :: [[Integer]]
-- q9c = [[x] | x <- [1..y], y <- [1..5] ] -- é o mesmo raciocínio da maneira abaixo 
q9c = [[1..x] | x <- [1..5]]


q9d :: [[Integer]]
-- q9d = [[1..x] | x <- [1,1,1,1,1]]
q9d = [replicate x 1 | x <- [1..5] ]
-- replicate :: Int -> a -> [a]

-- [1,2,6,24,120,720]
q9e :: [Integer]
-- q9e =  [fac x | x <- [1,2,3,4,5,6]]
q9e = [product [y | y <- [1..x]] | x <- [1..6]]
-- product calcula a soma de todos os elementos de uma lista 

fac :: Integer -> Integer
fac n | n < 2     = 1
      | otherwise = n * fac (n-1)

