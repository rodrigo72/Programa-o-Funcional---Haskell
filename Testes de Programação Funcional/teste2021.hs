module Teste2021 where 

{-
Apresente uma defini ̧c ̃ao recursiva da fun ̧c ̃ao (\\) :: Eq a => [a] -> [a] -> [a] que retorna a lista
resultante de remover (as primeiras ocorrˆencias) dos elementos da segunda lista da primeira. Por exemplo,
(\\) [1,2,3,4,5,1,2] [2,3,4,1,2] == [5,1].
-}

q1 :: Eq a => [a] -> [a] -> [a]
q1 [] l = [] 
q1 (h:t) l 
    | h `elem` l = q1 t (delete h l)
    | otherwise = h : (q1 t l)

delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (h:t)
    | x == h = t
    | otherwise = h : (delete x t)

{-
Considere o tipo MSet a para representar multi-conjuntos de elementos de a: type MSet a = [(a,Int)]
Considere ainda que nestas listas n ̃ao h ́a pares cuja primeira componente coincida, nem cuja segunda
componente seja menor ou igual a zero.

Defina a fun ̧c ̃ao removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que remove um ele-
mento a um multi-conjunto. Se o elemento n ̃ao existir, deve ser retornado o multi-conjunto recebido.
Por exemplo, removeMSet 'c' [('b',2), ('a',4), ('c',1)] == [('b',2), ('a',4)].
-}

type MSet a = [(a,Int)]

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = [] 
removeMSet a1 ((a2,n):t)
    | a1 == a2 && n == 1 = removeMSet a1 t 
    | a1 == a2 && n > 1  = (a2,(n-1)) : (removeMSet a1 t)
    | otherwise = (a2,n) : (removeMSet a1 t)

{-
Usando uma fun ̧c ̃ao de ordem superior, defina a fun ̧c ̃ao calcula :: MSet a -> ([a],Int) que,
numa  ́unica travessia do multi-conjunto, calcula simulanemente a lista (sem repetidos) de elementos
do multi-conjunto e o n ́umero total de elementos. Por exemplo, calcula [(’b’,2), (’a’,4),
(’c’,1)] == ([’b’,’a’,’c’],7).
-}

calcula :: MSet a -> ([a],Int)
calcula [] = ([],0)
calcula l@((a,n):t) = (a:x, foldr ((+) . snd) 0 l)
    where (x,y) = calcula t 

calcula' :: MSet a -> ([a],Int) 
calcula' l@((a,n):t) = foldr (\(ac1,ac2) (a,n) -> ((ac1 : a), (ac2 + n))) ([],0) l

{-
Defina a fun ̧c ̃ao partes :: String -> Char -> [String], que parte uma string pelos pontos onde um
dado caracter ocorre. Por exemplo, partes "um;bom;exemplo;" ’;’ == ["um","bom","exemplo"] e
partes "um;exemplo;qualquer" ’;’ == ["um","exemplo","qualquer"].
-}

partes :: String -> Char -> [String]
partes [] c = []
partes l@(h:t) c = takeWhile (/=c) l : (partes (semCabeça (dropWhile (/=c) l)) c)

semCabeça :: String -> String
semCabeça [] = []
semCabeça (h:t) = t 


{-
Considere o seguinte tipo para representar  ́arvores bin ́arias de procura.
-}

data BTree a = Empty 
             | Node a (BTree a) (BTree a)

a1 = Node 5 (Node 3 Empty Empty)
            (Node 7 Empty (Node 9 Empty Empty))

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty 
remove n (Node x e d)   
    | n == x = Empty
    | n > x  = (Node x e (remove n d))
    | n < x  = (Node x (remove n e) d)

{-
Defina BTree a como uma instˆancia da classe Show de forma a que show a1 produza a string
"((* <-3-> *) <-5-> (* <-7-> (* <-9-> *)))"
-}

instance (Eq a) => Eq (BTree a) where 
    Empty == Empty = True
    Empty == _     = False
    _ == Empty     = False
    Node x xl xr == (Node y yl yr) = (x == y) && xl == yl && xr == yr

-- instance (Show a, Eq a) => Show (BTree a) where 
--     show (Empty) = "*"
--     show (Node x e d) 
--         | e /= Empty && d /= Empty = "(" ++ "(" ++ show (e) ++ ")" ++ " <-" ++ show (x) ++ "-> " ++ "(" ++ show (d) ++ ")" ++ ")"
--         | e /= Empty && d == Empty = "(" ++ show (e) ++ ")" ++ " <-" ++ show (x) ++ "-> " ++ "*"
--         | e == Empty && d /= Empty = "*" ++ " <-" ++ show (x) ++ "-> " ++ "(" ++ show (d) ++ ")"
--         | e == Empty && d == Empty = "*" ++ " <-" ++ show (x) ++ "-> " ++ "*"

instance Show a => Show (BTree a) where
    show Empty = "*"
    show (Node r e d) = "(" ++ show e ++ " <- " ++ show r ++ " -> " ++ show d ++ ")"

{-
Apresente uma defini ̧c ̃ao da fun ̧c ̃ao sortOn :: Ord b => (a -> b) -> [a] -> [a] que ordena uma
lista comparando os resultados de aplicar uma fun ̧c ̃ao de extrac ̧c ̃ao de uma chave a cada elemento de uma
lista. Por exemplo: sortOn snd [(3,1),(2,5),(1,2)] == [(3,1),(1,2),(2,5)].
-}

sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn p [] = [] 
sortOn f (h:t) = insere h (sortOn f t)
    where insere x [] = [x]
          insere x (a:b) = if f x > f a 
                           then a : insere x b -- ordem crescente
                           else x:a:b 

{-
Considere o seguinte tipo para representar um sistema hier ́arquico de ficheiros
data FileSystem = File Nome | Dir Nome [FileSystem]
type Nome = String
fs1 = Dir "usr" [Dir "xxx" [File "abc.txt", File "readme", Dir "PF" [File "exemplo.hs"]],
Dir "yyy" [], Dir "zzz" [Dir "tmp" [], File "teste.c"] ]

Defina a fun ̧c ̃ao fichs :: FileSystem -> [Nome], que lista o nome de todos os ficheiros de um
file system.
-}

data FileSystem = File Nome | Dir Nome [FileSystem] 

type Nome = String
fs1 = Dir "usr" [
                    Dir "xxx" [Dir "no" [], File "abc.txt", File "readme", Dir "PF" [Dir "oh facil" [File "noice"], File "exemplo.hs"]],
                    Dir "yyy" [], Dir "yes" [], Dir "zzz" [Dir "tmp" [], File "teste.c"] 
                ]

fichs :: FileSystem -> [Nome]
fichs (File n) = [n]
fichs (Dir x n) = x : fichs2 n 
    where fichs2 [] = []
          fichs2 (h:t) = fichs h ++ (fichs2 t)     

{-
Defina a fun ̧c ̃ao dirFiles :: FileSystem -> [Nome] -> Maybe [Nome] que lista o nome dos
ficheiros de um file system que est ̃ao numa determinada path. Se a path n ̃ao for v ́alida, a fun ̧c ̃ao deve
devolver Nothing. Por exemplo, dirFiles fs1 ["usr","xxx"] == Just ["abc.txt","readme"]
-}

dirFiles :: FileSystem -> [Nome] -> Maybe [Nome]
dirFiles _ [] = Nothing 
dirFiles (Dir x n) [h] 
    | x == h = Just (printFiles n)
    | otherwise = Nothing
dirFiles (Dir x n) (h:t)
    | x == h = dirFiles2 n t 
    | otherwise = Nothing 

dirFiles2 :: [FileSystem] -> [Nome] -> Maybe [Nome]
dirFiles2 [] _ = Nothing
dirFiles2 ((File n):xs) [h] = dirFiles2 xs [h]
dirFiles2 ((Dir x n):t) [h]
    | x == h = Just (printFiles n)
dirFiles2 ((File n):xs) l = dirFiles2 xs l 
dirFiles2 ((Dir x n):xs) l@(h:t) 
    | x == h = dirFiles2 n t 
    | otherwise = dirFiles2 xs l 

printFiles :: [FileSystem] -> [Nome]
printFiles [] = [] 
printFiles ((Dir x n):t) = printFiles t 
printFiles ((File n):t) = n : printFiles t 

nomesToString :: [Nome] -> String 
nomesToString [] = ""
nomesToString (h:t) = h ++ " " ++ nomesToString t 

listaFich :: FileSystem -> IO ()
listaFich fs = do putStrLn "Escreva uma diretoria:"
                  dir <- getLine 
                  case (dirFiles fs (partes dir '/')) of   
                      Nothing -> putStrLn "Nao e uma diretoria"
                      Just n  -> putStrLn (nomesToString n)

-- data FileSystem = File Nome | Dir Nome [FileSystem]

-- dirFiles :: FileSystem -> [Nome] -> Maybe [Nome]
-- dirFilesAux :: FileSystem -> [Nome] -> [Nome]
-- dirFilesAux (File x) [] = [x]
-- dirFilesAux (File x) t = []
-- dirFilesAux (Dir a []) t = []
-- dirFilesAux a [] = []
-- dirFilesAux (Dir x l) (h:t) = if h == x then concat [dirFilesAux (l!!a) t | a <- [0..length l - 1]] else []
-- dirFiles a b = if null (dirFilesAux a b) then Nothing else Just (dirFilesAux a b)

-- listaFich :: FileSystem -> IO ()
-- listaFich fs = do
--     path' <- getLine
--     if null (dirFilesAux fs (writePath path')) then print "Nao e uma diretoria" else print (dirFilesAux fs (writePath path'))

-- writePath :: String -> [String]
-- writePath [] = []
-- writePath [x] = if x == '/' then [] else [[x]]
-- writePath l
--     | not (elem '/' l) = [l]
--     | otherwise = takeWhile (/='/') l:writePath (tail (dropWhile (/='/') l ))
