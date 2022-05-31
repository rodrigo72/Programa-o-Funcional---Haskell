-- Resolução da ficha 6 de Programação Funcional 
-- Árvores Binárias com conteúdo nos nós
module Ficha6 where

data BTree a = Empty 
             | Node a (BTree a) (BTree a)
    deriving Show

{-
1. a) 

altura :: BTree a -> Int que calcula a altura da  ́arvore.
-}

arv = (Node 5 (Node 7 (Node 3 Empty Empty) 
                      (Node 2 (Node 10 Empty Empty) Empty)
              )
              (Node 1 (Node 12 Empty Empty)
                      (Node 4 Empty (Node 8 Empty Empty))
              )
      )

altura :: BTree a -> Int
altura Empty = 0 
altura (Node _ e d) = 1 + max (altura e) (altura e)

{-
1. b)

contaNodos :: BTree a -> Int que calcula o n ́umero de nodos da  ́arvore.
-}

contaNodos :: BTree a -> Int 
contaNodos Empty = 0 
contaNodos (Node x e d) = 1 + contaNodos e + contaNodos d 

{-
1. c)

folhas :: BTree a -> Int, que calcula o n ́umero de folhas (i.e., nodos sem de-
scendentes) da  ́arvore.
-}

folhas :: BTree a -> Int 
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d) = folhas e + folhas d 

{-
1. d)

prune :: Int -> BTree a -> BTree a, que remove de uma  ́arvore todos os el-
ementos a partir de uma determinada profundidade.
-}

prune :: Int -> BTree a -> BTree a
prune _ Empty        = Empty
prune 0 _            = Empty
prune n (Node x e d) = Node x (prune (n-1) e) (prune (n-1) d)

-- o 'e' e o 'd' são as ramificações, ou seja, também podem ser árvores ao ser retirada a raiz

{-
1. e)

path :: [Bool] -> BTree a -> [a], que dado um caminho (False corresponde
a esquerda e True a direita) e uma  ́arvore, d ́a a lista com a informa ̧c ̃ao dos nodos
por onde esse caminho passa.
-}

path :: [Bool] -> BTree a -> [a] 
path [] _ = []
path _ Empty = []
path (h:t) (Node x e d)
    | h == True = x : (path t d)
    | otherwise = x : (path t e)

{-
1. f)

mirror :: BTree a -> BTree a, que d ́a a  ́arvore sim ́etrica.
-}

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node e l r) = Node e (mirror r) (mirror l)

{-
1. g)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c que gener-
aliza a fun ̧c ̃ao zipWith para  ́arvores bin ́arias.
-}

{- Função de ordem superior zipWith 
   zipWith constroi uma lista cujos elementos sao calculados por uma 
   funçao que é aplicada a argumentos que vêm das DUAS listas.

   zipWith :: (a -> b -> b) -> [a] -> [b] -> [c]
   zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
   zipWith _ _ _ = [] 

-}

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c 
zipWithBT f (Node x1 e1 d1) (Node x2 e2 d2) = 
    Node (f x1 x2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT _ _ _ = Empty

{-
1. h)

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c), que generaliza a
fun ̧c ̃ao unzip (neste caso de triplos) para  ́arvores bin ́arias.
-}

unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) e d) = (
                                Node a unzipE1 unzipD1, 
                                Node b unzipE2 unzipD2, 
                                Node c unzipE3 unzipD3
                             )
    where (unzipE1, unzipE2, unzipE3) = unzipBT e
          (unzipD1, unzipD2, unzipD3) = unzipBT d 

-- Defina as seguintes funções, 
-- assumindo agora que as  ́arvores são binárias de procura

arv2 = (Node 5 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) 
                       (Node 4 Empty Empty)
              )
              (Node 10 (Node 7 Empty (Node 8 Empty Empty))
                       (Node 12 Empty Empty)
              )
      )

{-
2. a)

Defina uma fun ̧c ̃ao minimo :: Ord a => BTree a -> a que determina o menor
elemento de uma  ́arvore bin ́aria de procura n ̃ao vazia.
-}

minimo :: Ord a => BTree a -> a 
minimo (Node x Empty _) = x  
minimo (Node x e d) = minimo e 


{-
2. b)

Defina uma fun ̧c ̃ao semMinimo :: Ord a => BTree a -> BTree a que remove o
menor elemento de uma  ́arvore bin ́aria de procura n ̃ao vazia.
-}

semMinimo :: Ord a => BTree a -> BTree a 
semMinimo (Node x Empty _) = Empty
semMinimo (Node x e d) = Node x (semMinimo e) d 

{-
2. c)

Defina uma fun ̧c ̃ao minSmin :: Ord a => BTree a -> (a,BTree a) que calcula,
com uma  ́unica travessia da  ́arvore o resultado das duas fun ̧c ̃oes anteriores
-}

minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin (Node x Empty _) = (x, Empty)
minSmin (Node x e d) = (a, Node x b d)
    where (a,b) = minSmin e 
    -- bruh, isto tá mais para brainfuck do que para Haskell

{-
2. d)

Defina uma fun ̧c ̃ao remove :: Ord a => a -> BTree a -> BTree a que remove
um elemento de uma  ́arvore bin ́aria de procura, usando a fun ̧c ̃ao anterior.
-}

remove :: Ord a => a -> BTree a -> BTree a 
remove n (Node x e d)
    | x  > n = (Node x (remove n e) d)
    | x  < n = (Node x e (remove n d)) 
    | x == n = case e of Empty -> d 
                         _ -> case d of Empty -> e 
                                        _ -> Node g d h 
    where (g,h) = minSmin d 

    -- Não entendi muito bem o último caso 
    -- Explicação:
    {-
        Nesta função, depois de remover o elemento, 
        temos de formar uma nova árvore, pois não podemos ter um nodo vazio. 
        Para isso, removemos o menor elemento do ramo da direita e colocamos 
        esse elemento onde estava o elemento removido. Desta forma, a árvore 
        mantém a sua ordem, já que todos os elementos à esquerda continuam a 
        ser mais pequenos e todos os elementos à direita continuam a ser 
        maiores do que o elemento no nodo.
    -}

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String

data Regime = ORD | TE | MEL 
    deriving Show

data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show

type Turma = BTree Aluno 
--  ́arvore binária de procura (ordenada por número)

{-
3. a)

inscNum :: Numero -> Turma -> Bool, que verifica se um aluno, com um dado
n ́umero, est ́a inscrito.
-}

inscNum :: Numero -> Turma -> Bool 
inscNum n Empty = False 
inscNum n (Node (x,_,_,_) e d)
    | n  > x = inscNum n d 
    | n  < x = inscNum n e 
    | n == x = True 

turma :: Turma
turma = (Node (15,"Luis",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty Empty) (Node (14,"Lara",ORD,Aprov 19) Empty Empty)) (Node (20,"Pedro",TE,Aprov 10) Empty (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty Empty) (Node (28,"Vasco",MEL,Rep) Empty Empty))))

{-
Outra forma de fazer: 
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) l r) = n == num || inscNum n (if n < num then l else r)
-}

{-
3. b)

inscNome :: Nome -> Turma -> Bool, que verifica se um aluno, com um dado
nome, est ́a inscrito.
-}

inscNome :: Nome -> Turma -> Bool
-- inscNome n Empty = False
-- inscNome n (Node (_,nome,_,_) e d)
--     | n == nome = True 
--     | otherwise = case e of Empty -> inscNome n d 
--                             _ -> case d of Empty -> inscNome n e 
--                                            _ -> inscNome n d  

inscNome _ Empty = False
inscNome n (Node (_,nome,_,_) l r) = 
    n == nome || inscNome n l || inscNome n r

-- mais simples do que eu pensava 

{-
3. c)

trabEst :: Turma -> [(Numero,Nome)], que lista o n ́umero e nome dos alunos
trabalhadores-estudantes (ordenados por n ́umero).
-}

trabEst :: Turma -> [(Numero, Nome)]
trabEst Empty = []
trabEst (Node (numero,nome,reg,_) e d) = 
    case reg of TE -> [(numero, nome)] ++ (trabEst d) ++ (trabEst e)
                _  -> (trabEst d) ++ (trabEst e)

{-
3. d)

nota :: Numero -> Turma -> Maybe Classificacao, que calcula a classifica ̧c ̃ao
de um aluno (se o aluno n ̃ao estiver inscrito a fun ̧c ̃ao deve retornar Nothing).
-}

nota :: Numero -> Turma -> Maybe Classificacao 
-- nota n arv@(Node (numero,_,_,classificacao) e d)
--     | (inscNum n arv) = if n == numero 
--                         then Just classificacao 
--                         else case e of Empty -> nota n d 
--                                        _     -> nota n e 
--     | otherwise = Nothing

nota n arv@(Node (numero,_,_,classificacao) e d) 
    | n == numero = Just classificacao
    | n  < numero = nota n e 
    | otherwise = nota n d

nota _ _ = Nothing 

{-
3. e)

percFaltas :: Turma -> Float, que calcula a percentagem de alunos que fal-
taram `a avalia ̧c ̃ao.
-}

percFaltas :: Turma -> Float
percFaltas Empty = 0 
percFaltas tree = ((aux tree) / (fromIntegral (contaNodos tree))) * 100 
    where aux :: Turma -> Float
          aux Empty = 0 
          aux (Node (_,_,_,classificacao) e d) = 
              case classificacao of Faltou -> 1 + (aux e) + (aux d)
                                    _      -> (aux e) + (aux d)

{-
3. f)

mediaAprov :: Turma -> Float, que calcula a m ́edia das notas dos alunos que
passaram.
-}

-- mediaAprov :: Turma -> Float
-- mediaAprov Empty = 0 
-- mediaAprov tree = mediaAprovAc tree 0 
--     where mediaAprovAc :: Turma -> Float -> Float
--           mediaAprovAc (Node (_,_,_,Aprov notas) e d) ac =  
--               (ac) / (1 + (mediaAprov)) 

{-
3. g)


-}
