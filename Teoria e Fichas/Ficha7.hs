-- Resolução da ficha 7 de Programação Funcional 
-- Outros tipos de árvores
module Ficha7 where

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

{-
1. a)

Os termos deste tipo ExpInt podem ser vistos como  ́arvores cujas folhas s ̃ao inteiros e
cujos nodos (n ̃ao folhas) s ̃ao operadores.

Defina uma fun ̧c ̃ao calcula :: ExpInt -> Int que, dada uma destas express ̃oes
calcula o seu valor.
-}

calcula :: ExpInt -> Int 
calcula (Const num) = num 
calcula (Simetrico exp) = (- calcula exp)
calcula (Mais  a b) = (calcula a) + (calcula b)
calcula (Menos a b) = (calcula a) - (calcula b)
calcula (Mult  a b) = (calcula a) * (calcula b)

{-
1. b)

Defina uma função infixa :: ExpInt -> String de forma a que infixa (Mais (Const 3) (Menos (Const 2) (Const 5))) 
dê como resultado "(3 + (2 - 5))".
-}

infixa :: ExpInt -> String 
infixa (Const num) = show num 
infixa (Simetrico exp) = "(-(" ++ infixa exp ++ "))"
infixa (Mais  a b) = '(':infixa a ++ " + " ++ infixa b ++ ")"
infixa (Menos a b) = '(':infixa a ++ " - " ++ infixa b ++ ")"
infixa (Mult  a b) = '(':infixa a ++ " * " ++ infixa b ++ ")"

{-
1. c)

Defina uma outra função de conversão para strings posfixa :: ExpInt -> String 
de forma a que quando aplicada à expressão acima dˆe como resultado "3 2 5 - +".
-}

posfixa :: ExpInt -> String
posfixa (Const num) = show num ++ " "
posfixa (Simetrico exp) = '-':posfixa exp
posfixa (Mais a b) = posfixa a ++ posfixa b ++ "+ "
posfixa (Menos a b) = posfixa a ++ posfixa b ++ "- "
posfixa (Mult a b) = posfixa a ++ posfixa b ++ "* "

{-
2. a)

soma :: Num a => RTree a -> a que soma os elementos da  ́arvore
-}

data RTree a = R a [RTree a]
    deriving (Show)

arv = R 5 [ R 4 [ R 3 [R 17 []], R 2 [], R 7 []],
            R 10 [],
            R 1 [ R 8 [ R 0 [], R 20 [], R 15 [],  R 39 [] ],
                  R 12 [] ]
          ] 

-- Visualização: 2021-12-23-13-36-06.png

soma :: Num a => RTree a -> a 
soma (R x []) = x 
soma (R x l)  = x + sum (map soma l)

{-
2. b)

altura :: RTree a -> Int que calcula a altura da  ́arvore.
-}

altura :: RTree a -> Int 
altura (R x []) = 1
altura (R x l)  = 1 + maximum (map altura l)

{-
2. c)

prune :: Int -> RTree a -> RTree a que remove de uma árvore todos os 
elementos a partir de uma determinada profundidade.
-}

{-
Uso do map:

mapBT :: (a -> b) -> BTree a -> BTree b 
         mapBT Empty = Empty
         mapBT f (Node x e d) = Node (f x) (mapBT f e) (mapBT f d)

-}

prune :: Int -> RTree a -> RTree a
prune 0 (R x l) = R x []  
prune n (R x l) = R x (map (prune (n-1)) l)

{-
2. d)

mirror :: RTree a -> RTree a que gera a árvore simétrica.
-}

mirror :: RTree a -> RTree a 
mirror (R x l) = R x (map mirror (reverse l))


{-
2. e)

postorder :: RTree a -> [a] que corresponde à travessia postorder da árvore.
-}

postorder :: RTree a -> [a]
postorder (R e []) = [e]
postorder (R e es) = concatMap postorder es ++ [e]

{-
3. a)

ltSum :: Num a => LTree a -> a que soma as folhas de uma  ́arvore.

Relembre a defini ̧c ̃ao de  ́arvores bin ́arias apresentada na ficha anterior
Nestas  ́arvores a informa ̧c ̃ao est ́a nos nodos (as extermidades da  ́arvore tˆem apenas
uma marca – Empty).  ́E tamb ́em habitual definirem-se  ́arvores em que a informa ̧c ̃ao
est ́a apenas nas extermidades (leaf trees ):
-}

data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving (Show)

data LTree a = Tip a | Fork (LTree a) (LTree a)
    deriving (Show)

arv2 = Fork (Fork (Tip 5)
                    (Fork (Tip 6)
                          (Tip 4)))
              (Fork (Fork (Tip 3)
                          (Tip 7))
                    (Tip 5))

ltSum :: Num a => LTree a -> a 
ltSum (Tip n) = n 
ltSum (Fork a b) = ltSum a + ltSum b 

{-
3. b)

listaLT :: LTree a -> [a] que lista as folhas de uma árvore (da esquerda para a direita).
-}

listaLT :: LTree a -> [a] 
listaLT (Tip n) = [n]
listaLT (Fork a b) = listaLT a ++ listaLT b

{-
3. c)

ltHeight :: LTree a -> Int que calcula a altura de uma árvore.
-}

ltHeight :: LTree a -> Int 
ltHeight (Tip n) = 1 
ltHeight (Fork a b) = 1 + max (ltHeight a) (ltHeight b)

{-
4. a)

São as chamadas full trees onde a informação está não só nos nodos, como também nas folhas (note que o tipo da informação nos nodos e nas folhas não tem que ser o mesmo).
Defina a função splitFTree :: FTree a b -> (BTree a, LTree b) que separa uma árvore com informação nos nodos e nas folhas em duas árvores de tipos diferentes.
-}

data FTree a b = Leaf b | No a (FTree a b) (FTree a b) 
    deriving (Show)


arv3 = No 8 (No 1 (Leaf 5)
                    (No 2 (Leaf 6)
                          (Leaf 4)))
              (No 9 (No 10 (Leaf 3)
                           (Leaf 7))
                    (Leaf 5))

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf n) = (Empty, Tip n)
splitFTree (No a b c) = (Node a b' c', Fork b'' c'')
    where (b', b'') = splitFTree b 
          (c', c'') = splitFTree c 

{-
4. b)

Defina ainda a função joinTrees :: BTree a -> LTree b -> Maybe (FTree a b) que sempre que as árvores sejam compatíveis as junta numa só.
-}

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip n) = Just (Leaf n)
joinTrees (Node e l r) (Fork a b) = Just (No e aux aux2)
    where Just aux = joinTrees l a
          Just aux2 = joinTrees r b
joinTrees _ _ = Nothing

