-- Resolução da ficha 5 de programação funcional 
module Ficha5 where 

{-
1. a)

any :: (a -> Bool) -> [a] -> Bool que teste se um predicado ́e verdade para algum elemento de uma lista; por exemplo:
any odd [1..10] == True

-}
any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False 
any' p (x:xs) = if (p x)  
                then True
                else any' p xs
-- p é a condição 

{-
1. b)

zipWith :: (a->b->c) -> [a] -> [b] -> [c] que combina os elementos de duas listas usando uma fun ̧c ̃ao espec ́ıfica; por exemplo:
zipWith (+) [1,2,3,4,5] [10,20,30,40] == [11,22,33,44].

-}

q1b :: (a -> b -> c) -> [a] -> [b] -> [c]
q1b p [] l = []
q1b p l [] = []
q1b p (x:xs) (y:ys) = (p x y) : (q1b p xs ys) 

{-
1. c) 

takeWhile :: (a->Bool) -> [a] -> [a] que determina os primeiros elementos da lista que satisfazem um dado predicado; por exemplo:
takeWhile odd [1,3,4,5,6,6] == [1,3].

-}

q1c :: (a -> Bool) -> [a] -> [a]
q1c p [] = []
q1c p (x:xs)
    | p x = x : q1c p xs 
    | otherwise = q1c p xs 

{-
1. d) 

dropWhile :: (a->Bool) -> [a] -> [a] que elimina os primeiros elementos da lista que satisfazem um dado predicado; por exemplo:
dropWhile odd [1,3,4,5,6,6] == [4,5,6,6].

-}

q1d :: (a -> Bool) -> [a] -> [a]
q1d p [] = []
q1d p (x:xs)
    | p x = q1d p xs
    | otherwise = x : q1d p xs 

{-
1. e) 

span :: (a-> Bool) -> [a] -> ([a],[a]), que calcula simultaneamente os dois resultados anteriores. Note que apesar de poder ser
definida `a custa das outras duas, usando a defini ̧c ̃ao
span p l = (takeWhile p l, dropWhile p l)
nessa defini ̧c ̃ao h ́a trabalho redundante que pode ser evitado. Apresente uma defini ̧c ̃ao alternativa onde n ̃ao haja 
duplicação de trabalho.

-}

q1e :: (a-> Bool) -> [a] -> ([a],[a]) 
q1e p [] = ([], [])
q1e p (x:xs) 
    | p x = (x:a, b)
    | otherwise = (a, x:b)
    where (a,b) = q1e p xs 

{-
1. f)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a] que apaga o primeiro el-
emento de uma lista que  ́e “igual” a um dado elemento de acordo com a fun ̧c ̃ao
de compara ̧c ̃ao que  ́e passada como parˆametro. Por exemplo:
deleteBy (\x y -> snd x == snd y) (1,2) [(3,3),(2,2),(4,2)]

-}

q1f :: (a -> a -> Bool) -> a -> [a] -> [a]
q1f p x (h:t) 
    | p x h = t 
    | otherwise = h : q1f p x t 

{-
1. g) 

sortOn :: Ord b => (a -> b) -> [a] -> [a] que ordena uma lista comparando os resultados de aplicar uma fun ̧c ̃ao de extracção 
de uma chave a cada elemento de uma lista. Por exemplo:
sortOn fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)].
-}

q1g :: (Ord b) => (a -> b) -> [a] -> [a]
q1g p [] = []
q1g p (h:t) = insere h (q1g p t)
    where insere x [] = [x]
          insere x (a:b) = if p x > p a 
                           then a : insere x b 
                           else x:a:b 

{-
2. a) 

[(2,3), (3,4), (5,3), (4,5)] representa o polin ́omio 2 x3 + 3 x4 + 5 x3 + 4 x5. 
Redefina as fun ̧c ̃oes pedidas nessa ficha, usando agora fun ̧c ̃oes de ordem superior 
(definidas no Prelude ou no Data.List) em vez de recursividade expl ́ıcita:

selgrau :: Int -> Polinomio -> Polinomio que selecciona os mon ́omios com um dado grau de um polin ́omio
-}

type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n l@((x,y):t) = filter (\(x,y) -> y == n) l

{-
2. b)

conta :: Int -> Polinomio -> Int de forma a que (conta n p) indica quan-
tos mon ́omios de grau n existem em p.

-}

conta :: Int -> Polinomio -> Int 
conta n l = length (selgrau n l)

conta1 :: Int -> Polinomio -> Int
conta1 n [] = 0 
conta1 n l@((x,y):t) = if n == y 
                       then 1 + conta1 n t 
                       else conta1 n t 

conta2 n l  = foldr (\(c,e) r -> if e == n then 1+r else r) 0 l 

{-
2. c)

grau :: Polinomio -> Int que indica o grau de um polin ́omio.

-}

grau :: Polinomio -> Int 
-- grau l = foldl (\(x,y) ac -> if ac >= y then ac else y) 0 l 
grau l = foldr (\(x,y) ac -> if ac >= y then ac else y) 0 l

{-
2. d)

deriv :: Polinomio -> Polinomio que calcula a derivada de um polin ́omio.
-}

deriv :: Polinomio -> Polinomio
deriv p = map (\(x,y) -> (x*(fromIntegral y), y-1)) p 

{-
2. e) 

calcula :: Float -> Polinomio -> Float que calcula o valor de um polin ́omio para uma dado valor de x.
-}

calcula :: Float -> Polinomio -> Float
calcula n p = foldl (\ac (x,y) -> ac + (x * (n^y))) 0 p

{-
2. f)

simp :: Polinomio -> Polinomio que retira de um polin ́omio os mon ́omios de coeficiente zero.
-}

simp :: Polinomio -> Polinomio
simp p = q1d (\(x,y) -> x == 0) p

{-
2. g) 

mult :: Monomio -> Polinomio -> Polinomio que calcula o resultado da multiplica ̧c ̃ao de um mon ́omio por um polin ́omio.
-}

mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) p = map (\(a,b) -> (x*a, y+b)) p 
--                     |-> função a ser aplicada aos elementos DA LISTA 
-- o 'p' pode ser omitido 

{-
2. h)

ordena :: Polinomio -> Polinomio que ordena um polon ́omio por ordem crescente dos graus dos seus mon ́omios.
-}

--ordena :: Polinomio -> Polinomio
--ordena (h:t) = q1g snd

{-
2. i)

normaliza :: Polinomio -> Polinomio que dado um polin ́omio constr ́oi um polin ́omio equivalente em que n ̃ao podem aparecer 
varios mon ́omios com o mesmo grau.
-}

normaliza :: Polinomio -> Polinomio
-- normaliza p = foldl (\ac (x,y) -> oh (selgrau y p)) [] p 
normaliza [] = []
-- normaliza ((b,e):ps) = (sum [bs | (bs,es) <- selgrau e ps] + b,e):normaliza [(bo,eo) | (bo,eo) <- ps, eo /= e]
normaliza p@((x,y):t) = ((oh (selgrau y p)), y) : normaliza (q1d (\(a,b) -> b == y) t)

oh :: Polinomio -> Float
oh [] = 0
oh ((x,y):t) = x + (oh t)

{-
2. j)

soma :: Polinomio -> Polinomio -> Polinomio que faz a soma de dois polin ́omios
de forma que se os polin ́omios que recebe estiverem normalizados produz tamb ́em
um polin ́omio normalizado.

-}

soma :: Polinomio -> Polinomio -> Polinomio
soma p r = normaliza $ (++) p r

{-
2. k) 

produto :: Polinomio -> Polinomio -> Polinomio que calcula o produto de dois polin ́omios
-}

produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldl (\ac x -> soma (mult x p2) ac) [] p1 

pol :: Polinomio
pol = [(1,2), (1,3), (1,2), (4,3), (1,4), (0,1)]



