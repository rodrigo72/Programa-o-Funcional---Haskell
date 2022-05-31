module Teste1920 where 
-- Teste de 

{-
inits:: [a] -> [[a]] que calcula a lista dos prefixos de uma lista. Por exemplo, inits [11,21,13]
corresponde a [[],[11],[11,21],[11,21,13]].
-}

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

{-
isPrefixOf:: Eq a => [a] -> [a] -> Bool que testa se uma lista  ́e prefixo de outra. Por ex-
emplo, isPrefixOf [10,20] [10,20,30] corresponde a True enquanto que isPrefixOf [10,30]
[10,20,30] corresponde a False.
-}

q2 :: Eq a => [a] -> [a] -> Bool
q2 [] _ = True
q2 _ [] = False 
q2 (x:xs) (y:ys) 
    | x == y = q2 xs ys 
    | otherwise = False 

{-
Defina a fun ̧c ̃ao folhas :: BTree a -> Int, que calcula o n ́umero de folhas (i.e., nodos sem de-
scendentes) da  ́arvore.
-}

data BTree a = Empty
             | Node a (BTree a) (BTree a)
    deriving Show

folhas :: BTree a -> Int
folhas (Node a Empty Empty) = 1 
folhas Empty = 0 
folhas (Node a e d) = folhas e + folhas d 

{-
Defina a fun ̧c ̃ao path :: [Bool] -> BTree a -> [a], que dado um caminho (False corresponde
a esquerda e True a direita) e uma  ́arvore, d ́a a lista com a informa ̧c ̃ao dos nodos por onde esse
caminho passa.
-}

arv = (Node 5 (Node 7 (Node 3 Empty Empty) 
                      (Node 2 (Node 10 Empty Empty) Empty)
              )
              (Node 1 (Node 12 Empty Empty)
                      (Node 4 Empty (Node 8 Empty Empty))
              )
      )

path :: [Bool] -> BTree a -> [a]
path [] _ = [] 
path _ Empty = [] 
path (h:t) (Node x e d) 
    | h == True  = x : path t e 
    | h == False = x : path t d 


{-
Uma representa ̧c ̃ao poss ́ıvel de polim ́omios  ́e pela sequˆencia dos
coeficientes - tˆem que se armazenar tamb ́em os coeficientes nulos
pois ser ́a a posi ̧c ̃ao do coeficiente na lista que dar ́a o grau do
mon ́omio.

A representa ̧c ̃ao do polin ́omio 2 x5 −5 x3 ser ́a ent ̃ao [0,0,0,-5,0,2], que corresponde ao polin ́omio
0 x0 + 0 x1 + 0 x2 −5 x3 + 0 x4 + 2 x5. Nas quest ̃oes que se seguem, use sempre que poss ́ıvel, fun ̧c ̃oes de
ordem superior.

Defina a opera ̧c ̃ao valor :: Polinomio -> Float -> Float que calcula o valor do polin ́omio para
um dado x.
-}

type Polinomio = [Coeficiente]
type Coeficiente = Float

valor :: Polinomio -> Float -> Float 
-- valor l n = foldr (\x@(last l) ac -> n^(length l) * x) 0 l
valor l n = sum [(l!!x)*(n^x) | x <- [1..(length l - 1)]]


{-
Defina a opera ̧c ̃ao deriv :: Polinomio -> Polinomio que calcula a derivada de um polin ́omio.
-}

-- [1,0,0,2] -> 1*n

deriv :: Polinomio -> Polinomio
deriv l = deriv2 l [1..] 
deriv2 :: Polinomio -> [Float] -> Polinomio 
deriv2 [] _ = [] 
deriv2 l@(h:t) (h1:t1)
    | h == 1 = deriv2 t t1 
    | h == 2 = h : deriv2 t t1 
    | otherwise = (h * h1) : deriv2 t t1

{- Defina a opera ̧c ̃ao soma :: Polinomio -> Polinomio -> Polinomio de adi ̧c ̃ao de polin ́omios-}

soma :: Polinomio -> Polinomio -> Polinomio
soma l1 l2 = zipWith (+) l1 l2 

type Mat a = [[a]]

-- quebraLinha :: [Int] -> [a] -> [[a]] 
-- quebraLinha [0] _ = []
-- quebraLinha (0:t) l = [] ++ (quebraLinha t l)
-- quebraLinha (h:t) (h1:t1) = h1 : quebraLinha ((h-1):t) t1

