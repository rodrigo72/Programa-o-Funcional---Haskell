module Teste1819 where 

import Data.Char
import System.Random 
import System.IO

-- 1. a)

elemIndices :: Eq a => a -> [a] -> [Int] 
elemIndices n l = elemIndices2 n l 0 []
    where elemIndices2 :: Eq a => a -> [a] -> Int -> [Int] -> [Int]
          elemIndices2 n [] ac1 ac2 = ac2
          elemIndices2 n (h:t) ac1 ac2 
            | n == h = ac1 : elemIndices2 n t (ac1+1) ac2
            | otherwise    = elemIndices2 n t (ac1+1) ac2

{-

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x (h:t)
    | x == h = 0 : map (+1) (elemIndices' x t)
    | otherwise = map (+1) (elemIndices' x t)

-}

-- 1. b) 

isSubquenceOf :: Eq a => [a] -> [a] -> Bool 
isSubquenceOf _ [] = False 
isSubquenceOf (x:xs) (y:ys) 
    | x == y = True && verificaResto xs ys 
    | otherwise = isSubquenceOf xs (y:ys)
    where verificaResto [] _ = True 
          verificaResto (x:xs) (y:ys) 
            | x == y = True && verificaResto xs ys 
            | otherwise = False 

{-

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h:t) (h':t') = h == h' && isSubsequenceOf t t' || isSubsequenceOf (h:t) t'

-}

-- 2. a) 

data BTree a = Empty | Node a (BTree a) (BTree a)

lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b 
lookupAP _ Empty = Nothing 
lookupAP n (Node (a,b) e d) 
    | n == a = Just b 
    | n > a = lookupAP n d 
    | otherwise = lookupAP n e 


-- 2. b) 

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c 
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node x e d) (Node x1 e1 d1) = Node (f x x1) (zipWithBT f e e1) (zipWithBT f d d1)

-- 3. 

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t)
    | ord h >= 48 && ord h <= 57 = (h:a,b)
    | otherwise = (a,h:b)
    where (a,b) = digitAlpha t

digitAlpha' :: String -> (String,String)
digitAlpha' l = foldr (\x (ac1,ac2) -> if isDigit x then (x:ac1,ac2) else (ac1,x:ac2)) ([],[]) l 

-- 4. a) 

data Seq a = Nil 
           | Cons a (Seq a) 
           | App (Seq a) (Seq a)

firstSeq :: Seq a -> a 
firstSeq (Cons a _)  = a 
firstSeq (App Nil s) = firstSeq s 
firstSeq (App _ s)   = firstSeq s 

-- 4. b) 

dropSeq :: Int -> Seq a -> Seq a 
dropSeq _ Nil = Nil 
dropSeq n seq@(Cons a b) = dropSeq (n-1) b 

dropSeq n (App s1 s2) 
    | n > nx    = dropSeq (n-nx) s2
    | n == nx   = s2 
    | otherwise = (App (dropSeq (n-1) s1) s2)
    where nx = contaCons s1 

contaCons :: Seq a -> Int
contaCons Nil = 0 
contaCons (Cons _ s)  = 1 + contaCons s 
contaCons (App s1 s2) = contaCons s1 + contaCons s2 

-- 5. c) 

instance (Show a) => Show (Seq a) where 
    show x = "<< " ++ mostraX x ++ " >>"

mostraX :: Show a => Seq a -> String 
mostraX Nil = ""
mostraX (Cons a Nil) = (show a) 
mostraX (Cons a x)  = (show a) ++ ", " ++ mostraX x
mostraX (App x1 x2) = mostraX x1 ++ ", " ++ mostraX x2 

-- 5. a) 

type Mat a = [[a]]

mostra :: Mat a -> Int -> a 
mostra (h:t) 1 = if not (null h) then head h else mostra t 1 
mostra l@(h:t) n = 
    case h of []      -> mostra t n 
              (h1:t1) -> mostra (t1:t) (n-1)



getElem :: Mat a -> IO a 
getElem mat@(h:t) = do 
    let linhas  = length mat 
    let colunas = length h 
    i <- randomRIO (1,linhas*colunas)
    let x = mostra mat i
    return x 

magic :: Mat Int -> Bool 
magic l = verificaLinhas l && verificaColunas l && verificaDiagonais l 

    -- verificaLinhas :: Mat Int -> Bool 
    -- verificaLinhas l@(h:t) = if null (filter (/=sum h) (foldr (\x ac -> sum x) [] l)) then True else False 

verificaLinhas :: Mat Int -> Bool
verificaLinhas l@(h:t) 
    | filter (== sum h) (map sum l) == map sum l = True
    | otherwise = False 

verificaColunas :: Mat Int -> Bool 
verificaColunas l@(h:t)
    | filter (== sum (map (!!0) l)) a == a = True 
    | otherwise = False 
    where a = [sum (map (!!x) l)  | x <- [0..length l -1]]

verificaDiagonais :: Mat Int -> Bool
verificaDiagonais l@(h:t)
    | sum [(map (!!x) l) !! x | x <- [0..(length h)-1]] == sum [(map (!!x) l) !! (length h-1-x) | x <- [0..length h -1]] = True  
    | otherwise = False
