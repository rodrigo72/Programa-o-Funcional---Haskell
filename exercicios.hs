module Ficha where

import Data.Char

perimetro :: Float -> Float
perimetro r = 2 * pi * r

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

primULT :: [Int] -> (Int, Int)
primULT l = (head l, last l)

-- max2 :: Int -> Int -> Int 
max2 x y = if x > y then x else y

-- Calcula o número de raizes reais de um polinómio de 2º grau 
nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c = if (b*b - 4*a*c) < 0
                then 0 
                else if (b^2 - 4*a*c) == 0 
                    then 1 
                    else 2

raizes :: Double -> Double -> Double -> [Double]
raizes a b c = if (nRaizes a b c) == 0
               then []
               else if (nRaizes a b c) == 1
                   then [ -b/(2*a) ]
                   else [((-b + sqrt(b^2 - 4*a*c)))/(2*a) , 
                        ((-b - sqrt(b^2 - 4*a*c)))/(2*a)]

-- 3
type Hora = (Int, Int)
-- a)
valida :: Hora -> Bool
valida (h,m) = h >= 0 && h < 24 && m >= 0 && m < 60

minutos :: Hora -> Int 
minutos (h,m) = 60*h + m 

-- 4
data HORA = H Int Int
    deriving (Show, Eq)

validaH :: HORA -> Bool
validaH (H x y) = x >= 0 && x < 24

truncaImpar :: [Int] -> [Int]
truncaImpar l = if mod (length l) 2 == 0
                then l
                else (tail l)

-- b)
validaC :: Hora -> Hora -> Bool
validaC (h1,m1) (h2,m2) = h1 > h2 && m1 > m2 

diferenca :: Hora -> Hora -> Int
diferenca (h1, m1) (h2, m2) = if (h1 - h2) * 60 + (m1 - m2) < 0
                              then -1 * ((h1 - h2) * 60 + (m1 - m2))
                              else (h1 - h2) * 60 + (m1 - m2) 

addH :: Hora -> Int -> Hora
addH (h, m1) m2 = if m1 + m2 < 60
                        then (h, m1 + m2)
                        else if m1 + m2 == 60 
                             then (h + 1, 0)
                             else (h + div (m1 + m2) 60, mod (m1 + m2) 60)

data Semaforo = Verde | Amarelo | Vermelho 
    deriving (Show,Eq)

next :: Semaforo -> Semaforo
next x = if x == Verde
             then Amarelo
             else if x == Amarelo 
                  then Vermelho
                  else Verde


stop :: Semaforo -> Bool
stop x 
    | x == Vermelho = True
    | otherwise = False 

safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = s1 == Verde && s2 == Verde 

data Ponto = Cartesiano Double Double | Polar Double Double
             deriving (Show, Eq)

posx :: Ponto -> Double
posx ponto = case ponto of Cartesiano x y -> x
                           Polar r a -> if a == pi/2
                                        then 0
                                        else r * cos a

posy :: Ponto -> Double
posy ponto = case ponto of Cartesiano x y -> y
                           Polar r a -> if a == pi
                                        then 0
                                        else r * sin a

-- 7

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show, Eq)

-- poligno :: Figura -> Bool

myisLower :: Char -> Bool
myisLower c = ord c >= 97 && ord c <= ord 'z'

--inToDigit :: Int -> Char 
-- inToDigit a | 0 <= a && <=  chr (a+48)

dobros :: [Float] -> [Float]
dobros [] = []
dobros (a:b) = a*2 : dobros b

duplica :: [a] -> [a]
duplica [] = []
duplica (a:b) = a : a : duplica b

pares :: [a] -> [(a,a)]
pares [] = []
pares [a] = []
pares (a:b:c) = (a,b) : pares c 

numOcorre :: Int -> [Int] -> Int
numOcorre x [] = 0 
numOcorre x (h:t) = if x == h
                    then 1  + numOcorre x t 
                    else numOcorre x t 

positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) | ((h > 0) == True) = positivos t 
                | otherwise         = False

positivos1 :: [Int] -> Bool
positivos1 [] = True
positivos1 (h:t) = h>0 && positivos1 t

tresUlt :: [a] -> [a]
tresUlt [x,y,z] = [x,y,z]
tresUlt [x,y] = [x,y]
tresUlt [x] = [x]
tresUlt [] = []
tresUlt (h:t) = tresUlt t

tresUlt1 [x,y,z] = [x,y,z]
tresUlt1 (a:b:c:d:xs) = tresUlt1 (b:c:d:xs)
tresUlt1 x = x

tresUlt2 :: [a] -> [a]
tresUlt2 [] = []
tresUlt2 (x:xs) | length (x:xs) <= 3 =  x:xs
                | otherwise         = tresUlt2 xs

tresUlt3 l = reverse (take 3 (reverse l))

--segundos :: [(a,b)] -> [b]
--segundos [] = []
-- segundos (h:t) = snd h : segundos t
--segundos ((x,y):t) = y : segundos 

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h > 0 then (h:(soPos t))
              else soPos t 

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if h > 0 
                then somaNeg t 
                else h + somaNeg t

count :: [Int] -> Int
count [] = 0 
count (h:t) = if (mod h 2) /= 0
              then 1 + count t 
              else count t

add :: Int -> [Int] -> [Int]
add x [] = []
add x (h:t) = ((h+x):(add x t))


rstring :: Char -> [[Char]] -> [[Char]]
rstring x [] = []
rstring x ((h:t):z) = if x == h 
                      then rstring x (z)
                      else ((h:t):(rstring x (z)))

-- Questão 1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b | a == b = [b]
                | otherwise = a: enumFromTo' (a+1) b 
{-
aux :: [(Int, Int)] -> Int -> Int 
aux [] x = x
aux ((_:b):t) x = if x > b then aux t x
                else aux t b 

maxPar' :: [(Int, Int)] -> Int
maxPar' ((_,b))
-}

maxPar :: [(Int,Int)] -> Int 
maxPar [] = 0 -- neste caso não é necessário
maxPar [(a,b)] = b -- caso de paragem
maxPar ((_,b):t) = if b > maxPar t then b 
                   else maxPar t

dois :: [(Int, Int)] -> [(Int,Int)]
dois [] = []
dois ((a,b):t) = ((a+2, b):(dois t))


maxxpar :: [(Int, Int)] -> Int
maxxpar [] = 0 
maxxpar [(a,b)] = b 
maxxpar ((a,b):t) = if b > maxxpar t then b 
                    else maxxpar t

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = (b:(segundos t))

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros x [] = False
nosPrimeiros x ((a,_):t) = if x == a then True
                           else nosPrimeiros x t 

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = (x+a, y+b, z+c)
                         where (a,b,c) = sumTriplos t

soDigitos :: [Char] -> [Char] 
soDigitos [] = []
soDigitos (h:t) = if ord h >= 48 && ord h <= 57 
                  then h:(soDigitos t)
                  else soDigitos t 

minusculas :: [Char] -> Int 
minusculas [] = 0        
minusculas (h:t) = if ord h >= 97 && ord h <= 122
                   then 1 + minusculas t
                   else minusculas t
{-                    
nums :: [[Char]] -> [Int]
nums [] = []
nums ((h:t):z) = if ord h >= 48 && ord h <= 57
                 then h:(nums t:z)
                 else nums t:z
-}

nums :: String -> [Int]
nums [] = []
nums (h:t) 
    | ord h > 47 && ord h < 58 = (ord h-48):nums t
    | otherwise = nums t
 
trocatroca :: [ohfacil] -> [ohfacil]
trocatroca [] = []
trocatroca (h:t) = trocatroca t ++ [h]

type Polinomio = [Monomio]
type Monomio = (Float, Int)

-- 4. a)
conta :: Int -> Polinomio -> Int
-- conta n p
-- quantos polinomios de grau n existem em p
conta n [] = 0
conta n ((a,b):t)  
    | n == b = 1 + conta n t 
    | otherwise = conta n t

-- b)
grau :: Polinomio -> Int
grau [] = 0 
grau ((_,b):t) = if b > grau t then b 
                 else grau t 

-- c) 
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((a,b):t) = if n == b then (a,b):(selgrau n t)
                      else selgrau n t 

deriv :: Polinomio -> Polinomio 
deriv [] = []
deriv ((a,b):t) = if b == 1 then (a,0):(deriv t)
                  else if b > 1 then (a * (fromIntegral b), b-1):(deriv t)
                       else (0,0):(deriv t)

-- f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,b):t) = if a == 0 then simp t
                 else (a,b):simp t

-- g) 
mult :: Monomio -> Polinomio -> Polinomio
mult (a,b) [] = []
mult (a,b) ((x,y):t) = (x*a, y+b):(mult (a,b) t)

-- h) 



normaliza :: Polinomio -> Polinomio
normaliza [(a,b)] = [(a,b)]
normaliza ((a,b):(a1,b1):t) 
    | b == b1 = normaliza ((a+a1, b):t)
    | conta b t == 0 = (a, b) : normaliza ((a1, b1):t)
    | otherwise = normaliza ((a,b):t) ++ [(a1,b1)] 

normaliza' :: Polinomio -> Polinomio
normaliza' [] = []
normaliza' (m:ms) = ins m (normaliza ms)

ins :: Monomio -> Polinomio -> Polinomio
ins (c,e) [] = [(c,e)]
ins (c,e) ((a,b):xs) = if e == b 
                         then (c+a, e):xs
                         else (a,b):ins (c,e) xs


{- Nop
ordenar :: Polinomio -> Polinomio
ordenar [] = []
ordenar ((a,b):(c,d):t) 
    | d > b = (a,b):(ordenar (c,d):t)
    | otherwise = (c:d):(ordenar ((a,b):t))
-}

-- i)
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

produto :: Polinomio -> Polinomio -> Polinomio 
produto [] [] = []
produto (h:t) t1 = soma (mult h t1) (produto t t1)

{-
ordena :: Ord a => [a] -> [a]
ordena [] = []
ordena(x:y:t)
    | y > x = y : ordena(x:t)
    | otherwise = x : ordena(y:t)
ordena(x:_) = [x]

verifica :: Ord a => [a] -> Bool
verifica [] = True
verifica [_] = True
verifica(x:y:t)
    | x > y = verifica(y:t)
    | otherwise = False 

funcOrd:: Ord a => [a] -> [a]
funcOrd x = if (verifica x) == False then funcOrd(ordena x) else x 
-}

-- ordena por orden crescente do grau 
ordena :: Polinomio -> Polinomio 
ordena [] = []
ordena (m:ms) = insere m (ordena ms)

insere :: Monomio -> Polinomio -> Polinomio
insere m [] = [m]
insere (c,e) ((a,b):xs) = if e <= b 
                          then (c,e):(a,b):xs 
                          else (a,b):insere (c,e) xs 

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 =  let p1n = (normaliza p1)
                   p2n = (normaliza p2)
                   p1nord = ordena p1n 
                   p2nord = ordena p2n
               in p1nord == p2nord

equiv' :: Polinomio -> Polinomio -> Bool
equiv' p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)

-- Ficha 3

-- 3) 

data Contacto = Casa Integer 
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving (Show, Eq)

type Nome = String 
type Agenda = [(Nome, [Contacto])]

ag1 :: Agenda 
ag1 = [("Ana", [Email "ana@gmail.com", Tlm 911345678, Casa 253123123]),
      ("Pedro", [Tlm 912123123]),
      ("Rui", [Trab 913222222, Email "rui@uminho.pt"]),
      ("Ines", []),
      ("Nuno", [Tlm 913222222, Tlm 914222222])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n s [] = [(n, [Email s])]
acrescEmail n s ((x,l):t)
        | n == x = if (Email s) `elem` l 
                   then ((x,l):t)
                   else (x, (Email s):l):t
        | otherwise = (x,l):(acrescEmail n s t) 


verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((x,l):t) 
        | n == x = Just (daEmails l)
        | otherwise = verEmails n t

daEmails :: [Contacto] -> [String]
daEmails [] = []
daEmails ((Email s):t) = s:(daEmails t)
daEmails (_:t) = daEmails t 

-- -----------
-- 50 Questões

enumFromTo'' :: Int -> Int -> [Int]
enumFromTo'' a b = if a == b then [a]
                   else a:(enumFromTo'' (a+1) b) 

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' a b c 
        | a > c && b - a > 0 || a < c && b - a < 0 = []
        | otherwise = a:enumFromThenTo' b (2 * a-b) c 

-- Aula LI

type Nome' = String
type Coordenada = (Int, Int)
data Movimento= N | S | E | W deriving (Show,Eq) -- norte, sul, este, oeste
type Movimentos = [Movimento]
data PosicaoPessoa = Pos Nome' Coordenada deriving (Show,Eq)

posicao :: PosicaoPessoa -> Movimentos -> PosicaoPessoa
posicao p [] = p 
posicao (Pos a (x,y)) (h:t) =
    case h of 
         N -> posicao (Pos a (x, y+1)) t 
         S -> posicao (Pos a (x, y-1)) t 
         E -> posicao (Pos a (x+1, y)) t 
         W -> posicao (Pos a (x-1, y)) t 

-- coordenadaMaisNorte :: [PosicaoPessoa] -> Int 
-- coordenadaMaisNorte [Pos _ (_,y)] = y 
-- coordenadaMaisNorte [(Pos _ (_,y)):ps] 
--     | pessoa > coordenada ps 

{-
posicoesM :: [PosicaoPessoa] -> Movimento -> [PosicaoPessoa]
posicoesM [] m = []
posicoesM (p:ps) m = posicao p [m] : posicoesM ps [m]


pessoasNorte :: [PosicaoPessoa] -> [Nome] 
pessoasNorte [] = []
pessoasNorte (Pos n (_, y):ps) = if y == max Norte
                                 then n
    where
        max Norte = max y (Pos n (x,y):ps)

max y :: [PosicaoPessoa] -> Int 
max y [Pos _ (_,y)] = y 
max y [Pos _ (_,y):ps] = if y >= max y ps 
                         then y 
                         else max y ps 

-}

-- 50 QUESTOES
-- 3.

questao3 :: [a] -> [a] -> [a]
questao3 [] l = l
questao3 (h:t) l = h:(questao3 t l)  

-- 4.
questao4 :: [a] -> Int -> a 
questao4 (h:t) x = if x == 0 
                   then h 
                   else questao4 t (x-1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = questao3 (reverse' t) [h]

take' :: Int -> [a] -> [a]
take' n (h:t) =  if n == 0 
                 then []
                 else h:(take' (n-1) t)

drop' :: Int -> [a] -> [a]
drop' n (h:t) = if n == 0 
                then (h:t)
                else drop' (n-1) t

zip' :: [a] -> [b] -> [(a,b)]
zip' [] l = []
zip' l [] = []
zip' (h:t) (h1:t1) = (h,h1):(zip' t t1)

replicate' :: Int -> a -> [a] 
replicate' n x = if n == 0 
                 then []
                 else x:(replicate' (n-1) x) 

intersperse' :: a -> [a] -> [a] 
intersperse' _ [] = []
intersperse' _ [h] = [h]
intersperse' x (h:t) = h:x:(intersperse' x t)

--group' :: Eq a => [a] -> [[a]] 
--group' (h:h1:t) 

-- oh :: Eq a => [a] -> [[a]]
-- oh [x] = [[x]]
-- oh [] = []
-- oh (h:h1:t) = if h == h1
--               then [take 2 h:h1:t] ++ oh h1:t
--               else  [[h]] ++ (oh h1:t)

teste :: Eq a => [a] -> [[a]]   
teste (h:h1:t) = [h1:t]
-- [[h], [h1]]++[t]

-- oh1 :: [[Int]] -> [[Int]]
-- oh1 ((h:t):(h1:t1):t2) = if t /= h1
--                          then oh1 (h1:t1):t2 
--                          else (h:t)++(h1:t1)(oh1 t2)
    
-- group' :: Eq a => [a] -> [[a]] 
-- group' [] = []
-- group' (h:h1:t) = if h == h1 
--                   then [[h,h1]] ++ group' (h1:t)
--                   else [[h], [h]] ++ group'(h1:t)

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' (h:t)
    | h == head hr = (h : hr) : tr 
    | otherwise = [h] : hr : tr 
    where (hr:tr) = group' t


group'' :: Eq a => [a] -> [[a]]
group'' [] = []
-- aux -> função auiliar
group'' (x:xs) = (x:aux xs) : aux2 xs
    where aux (y:ys) =  if y == x 
                        then y:aux ys 
                        else []
          aux [] = []
          aux2 [] = []
          aux2 (z:zs) = if z == x 
                        then aux2 zs
                        else group'' (z:zs)
                        

oh2 :: Eq a => [a] -> [[a]] 
oh2 [] = []
oh2 (x:y) = [[x]] ++ oh2 y

-- Ficha 3

-- data Hora = H Int Int
--             deriving (Show)
-- type Etapa = (Hora,Hora)
-- type Viagem = [Etapa]

-- teste' :: [Etapa] -> Bool
-- teste' (((H x y), (H xs, ys)):t) 
--     | x < xs && x < 60 && xs < 60 = True && teste' t 
--     | x == xs && x < 60 && xs && y < ys = True && teste' t 
    
-- 50 Questões

concat' :: [[a]] -> [a]
concat' [] = []
concat' [[x]] = [x]
concat' (h:t) = h ++ concat' t
               
inits' :: [a] -> [[a]]
inits' [] = []
inits' [x] = [[], [x]]
inits' (x:xs) = [] : aux x (inits' xs)
          where aux a (h:t) = (a:h):aux a t 
                aux _ [] = []    

-- areaPolig :: Poligonal -> Double 
-- areaOilig l = aux (triangula l)
--      where aux :: [Figura] -> Double 
--            aux [] = 0 
--            aux (f:fs) = area f + aux fs


-- 

data Movimento' = Credito Float | Debito Float
    deriving Show
data Data = D Int Int Int -- dia, mes, ano
    deriving Show
data Extracto = Ext Float [(Data, String, Movimento')]
    deriving Show

ext1 :: Extracto 
ext1 = Ext 1000 [(D 24 6 2020, "pagamento", Debito 25),
                 (D 1 7 2020, "pagamento 2", Debito 350),
                 (D 15 8 2021, "pagamento 3", Credito 150),
                 (D 14 9 2020, "pagamento 4", Debito 29) 
                ]

extValor :: Extracto -> Float -> [Movimento']
extValor (Ext s l) x = daMov l x 

daMov :: [(Data, String, Movimento')] -> Float -> [Movimento']
daMov ((_,_, Credito y):t) x = if y >= x 
                              then (Credito y):daMov t x
                              else daMov t x

daMov ((_,_, Debito y):t) x  = if y >= x 
                               then (Debito y):daMov t x
                               else daMov t x 

daMov [] _ = []

filtro :: Extracto -> [String] -> [(Data, Movimento')]
filtro (Ext _ m) l = aux m l 
aux [] _ = []
aux ((a,b,c):t) l 
    | b `elem` l = (a,c) : aux t l 
    | otherwise = aux t l 

-- creDeb :: Extracto -> (Float, Float)
-- creDeb (Ext _ m) = cdAux m 

-- cdAux :: [(Data, String, Movimento)] -> (Float, Float)
-- cdAux [] = (0,0)
-- cdAux ((_,_, Credito x):t) = (a+x, b)
--     where (a,b) = cdAux t 
-- cdAux ((_,_, Debito x):t) = (a, b+x)
--     where (a,b) = cdAux t 

-- saldo :: Extracto -> Float 
-- saldo (Ext s m) = let (c,d) = creDeb (Ext s m)
--                   in s + c - d 

nzp :: [Int] -> (Int, Int, Int)
nzp [] = (0,0,0)
nzp (x:xs) 
    | x > 0 = (a, b, c + 1)
    | x < 0 = (a + 1, b, c)
    |otherwise = (a, b + 1, c)
    where (a, b, c) = nzp xs

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t 
-- no ^

-- fromDigits' :: [Int] -> Int 
-- fromDigits' = fromDigitsAC l 0
--     where fromDigitsAC :: [Int] -> Int -> Int
--           fromDigitsAC (x:xs) ac = fromDigitsAC xs (x + 10 * ac)
--           fromDigitsAC [] ac = ac 

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' :: Int -> Int 
fib' n = fibAC n (0,1)
    where fibAC :: Int -> (Int, Int) -> Int 
          fibAC 0 (a,b) = a 
          fibAC 1 (a,b) = b 
          fibAC n (a,b) = fibAC (n-1) (b, a+b)

q13 :: [a] -> [[a]]
q13 l = q13 (init l) ++ [l]

q14 :: [a] -> [[a]]
q14 [] = [[]]
q14 l = [l] ++ q14 (tail l)

qq14 :: [a] -> [[a]]
qq14 [] =[[]]
qq14 (h:t) = (h:t) : q14 t 

q15 :: [[a]] -> [a]
q15 [] = []
q15 ((x:y):xs) = x : q15 xs 

q16 :: [[a]] -> Int
q16 [] = 0 
q16 (x:t) = aux x + q16 t
      where aux :: [a] -> Int 
            aux [] = 0 
            aux (a:b) = 1 + aux b

q17 :: [(a,b,c)] -> [(a,c)]
q17 [] = []
q17 ((a,b,c):t) = [(a,c)] ++ q17 t

q18 :: [(String, b, c)] -> String 
q18 [] = []
q18 ((a,b,c):t) = a ++ q18 t

q19 :: Int -> Int -> [(String, Int)] -> [String]
q19 _ _ [] = []
q19 x y ((a,b):t) 
    | x - b <= y = a : q19 x y t 
    | otherwise = q19 x y t 

q20 :: Int -> Int -> [Int]
q20 n 1 = [1]
q20 x y 
    | y > 1 = q20 x (y-1) ++ [x^(y-1)]
    | otherwise = []
-- x e y pertencem a N

q21 :: Int -> Bool 
q21 2 = True
q21 x 
    | x > 2 = areyousureaboutthat x 2 
    | otherwise = False -- numeros negativos, 0 e 1

areyousureaboutthat :: Int -> Int -> Bool 
areyousureaboutthat x y 
    | y * y > x = True 
    | mod x y == 0 = False 
    | otherwise = areyousureaboutthat x (y + 1)
    -- where toInt :: Float -> Int
    --       toInt x = round x

q22 :: Eq a => [a]-> [a] -> Bool
q22 [] _ = True 
q22 _ [] = False 
q22 (x:xs) (y:ys)
    | x /= y = False 
    | otherwise = q22 xs ys 

-- prefixo : primeiros elementos de uma lista

-- Ficha 3 LI
-- 14 - 16, ed. 7 1.09

find :: Eq oh => oh -> [oh] -> Int
find x [] = error "ohfacil"
find x (h:t) = if x == h 
               then 0 
               else 1 + find x t 

q11 :: Eq a => [a] -> [[a]]
q11 [] = []
q11 (h:t) = aux' [h] t 
-- função auxiliar 

aux' :: Eq a => [a] -> [a] -> [[a]]
aux' x [] = [x]
aux' x (h:t)
    | head x == h = aux' (h:x) t 
    | otherwise = x : aux' [h] t  


-- q23 :: Eq a => [a] -> [a] -> Bool 
-- q23 [] _ = False 
-- q23 _ [] = True 
-- q23 l1 l2
--     | x /= y = False
--     | otherwise = q23 xs ys 
--     where (x:xs) = reverse'' l1
--           (y:ys) = reverse'' l2 

ultimo :: [a] -> a 
ultimo [x] = x
ultimo (x:xs) = ultimo xs

-- reverse'' :: [a] -> [a]
-- reverse'' (x:xs) = reverseAc l []

-- reverseAc :: [a] -> [a] -> [a]
-- reverseAc (x:xs) ac = reverseAc xs (x:ac)
-- reverseAc [] ac = ac

q23 :: Eq a => [a] -> [a] -> Bool
q23 _ [] = True 
q23 [] _ = False 
q23 l1 l2 = q22 ll1 ll2
    where ll1 = reverse l1 
          ll2 = reverse l2

-- q24 :: Eq a => [a] -> [a] -> Bool 
-- q24 _ [] = True 
-- q24 [] _ = True 
-- q24 (x:xs) l = aux3 x l && q24 xs 

-- aux3 :: Eq a => a -> [a] -> Bool 
-- aux3 

q24 :: Eq a => [a] -> [a] -> Bool 
q24 _ [] = False 
q24 [] _ = True 
q24 (x:xs) (y:ys) = x == y && q24 xs ys || q24 (x:xs) ys

pertence :: Eq a => a -> [a] -> Bool
pertence x [] = False 
pertence x (y:ys) = if x == y 
                    then True
                    else pertence x ys 

-- q25 :: Eq a => a -> [a] -> [Int] 
-- q25 x l = aux4 x l 0 
-- criar mais uma variavel para definir posiçoes


inverte :: [a] -> [a]
inverte l = inverteAc l []
    where inverteAc [] ac = ac
          inverteAc (x:xs) ac = inverteAc xs (x:ac) 

q11' :: Eq a => [a] -> [[a]]
q11' (h:t) = q11Ac [h] t 
-- mais valores de entrada

q11Ac :: Eq a => [a] -> [a] -> [[a]]
q11Ac x [] = [x]
q11Ac x (h:t)
    | head x == h = q11Ac (h:x) t 
    | otherwise = x: q11Ac [h] t  

q11'' :: Eq a => [a] -> [[a]]
q11'' (h:t) = q11Ac2 [h] t 
q11Ac2 :: Eq a => [a] -> [a] -> [[a]]
q11Ac2 x [] = [x]
q11Ac2 x (h:t)
    | head x == h = q11Ac2 (h:x) t 
    | otherwise = x : q11Ac2 [h] t 

q25 :: Eq a => a -> [a] -> [Int] 
q25 x l = q25Ac x l 0 
    where q25Ac :: Eq a => a -> [a] -> Int -> [Int]
          q25Ac x [] ac = []
          q25Ac x (h:t) ac 
            | x == h = ac : q25Ac x t (ac + 1)
            | otherwise = q25Ac x t (ac + 1)

q26 :: Eq a => [a] -> [a]
q26 [] = [] 
q26 (h:t) = if h `elem` t 
            then q26 t 
            else h : q26 t 

q27 :: Eq a => a -> [a] -> [a] 
q27 _ [] = []
q27 x (h:t) 
    | x == h = t 
    | otherwise = h : q27 x t 

q28 :: Eq a => [a] -> [a] -> [a]
q28 l1 l2 = q28Ac l1 l2 [] 
    where q28Ac :: Eq a => [a] -> [a] -> [a] -> [a]
          q28Ac (x:xs) (y:ys) ac 
                | x == y = q28Ac (ac++xs) ys []
                | x /= y = q28Ac xs (y:ys) (ac++[x]) 
          q28Ac (x:xs) [] [] = (x:xs)
          q28Ac (x:xs) [] ac = ac++(xs)
          q28Ac [] _ _ = []
                      
            -- | x == y = q28Ac [x] ys (ac++[y]) 
            -- | (y:ys) == [] = q28Ac xs ac ac 

            -- | (x:xs) == [] = q
            -- | l == [] = ac++(x:xs)

oh0 :: Eq a => [a] -> [a] -> [a]  -- tres barras para evitar conflitos com a funcao pre definida... caso isso fosse um risco...
oh0 [] _ = []
oh0 l [] = l  
oh0 (x:xs) (y:ys)
    | x==y = oh0 xs ys
    | otherwise = x:oh0 xs (y:ys)

q28' :: Eq a => [a] -> [a] -> [a]
q28' l [] = l 
q28' [] _ = []
q28' l (h:t) = q28' (q27 h l) t 

q29 :: Eq a => [a] -> [a] -> [a]
q29 l [] = l
q29 l (h:t)
    | h `elem` l = q28' l t 
    | otherwise = q28' (l ++ [h]) t 

-- q29 :: Ord a => [a] -> [a] -> [a]
-- q29 l1 l2 = q29Ac l1 l2 [] 
--     where q29Ac :: Ord a => [a] -> [a] -> [a] -> [a]
--           q29Ac (x:xs) (y:ys) ac    
--             | x >= y = q29Ac xs ys (ac++[x])
--             | otherwise = q29Ac xs (y:ys) (ac++[y]++(x:xs))
--           q29Ac [] _ ac = ac 

q30 :: Eq a => [a] -> [a] -> [a] 
q30 [] _ = []
q30 (h:t) l 
    | h `elem` l = h : q30 t l 
    | otherwise = q30 t l 

q31 :: Ord a => a -> [a] -> [a] 
q31 x (h:t)
    | x <= h = x:(h:t)
    | otherwise = q31 x t 

q31' :: Ord a => a -> [a] -> [a]
q31' x l = q31Ac x l []
    where q31Ac :: Ord a => a -> [a] -> [a] -> [a]
          q31Ac x [] ac = [x]
          q31Ac x (h:t) ac
            | x > h = q31Ac x t (ac++[h])
            | otherwise = (ac++[x]++(h:t))

-- q31'' :: Ord a => a -> [a] -> [a]
-- q31'' x [] = [x]
-- q31'' x (h:t) 
--     | x > h = h : insert 

q32 :: [String] -> String  
q32 l = q32Ac l [] 
    where q32Ac :: [String] -> String -> String
          q32Ac [] ac = ac 
          q32Ac (h:t) ac = q32Ac t (ac ++ " " ++ h)

q32' :: [String] -> String
q32' [] = ""
q32' (h:t) = h ++ (if t == [[]] then "" else " ") ++ q32' t 

q33 :: [String] -> String 
q33 [] = ""
q33 (h:t) = h ++ "\n" ++ q33 t 

q34 :: (Ord a, Num a) => [a] -> Int 
q34 [] = 0 
q34 [x] = 0 
q34 l = q34Ac l [0] (-1) (-1)
    where q34Ac :: (Ord a, Num a) => [a] -> [a] -> Int -> Int -> Int
          q34Ac [] l pos pos2 = pos
          q34Ac (h:t) ac pos pos2 = if h > (head ac)
                                    then q34Ac t (ac++[h]) (1 + pos2) (1 + pos2)
                                    else q34Ac (ac++t) (ac++[h]) (-1) (1 + pos2)
            -- NAO FUNCIONA DESTA FORMA
            -- | h >= ac == q34Ac t (ac+1) 
            -- | otherwise = q34Ac t ac 

q34' :: Ord a => [a] -> Int 
q34' [_] = 0
q34' (h:t) 
    | h >= (t !! x) = 0 
    | otherwise = x + 1 
    where x = q34' t 

q35 :: Eq a => a -> [(a,b)] -> Maybe b 
q35 _ [] = Nothing
q35 x ((a,b):t) 
    | x == a = Just b 
    | otherwise = q35 x t 

q36 :: Ord a => [a] -> [a]
q36 [] = []
q36 [x] = [x]
q36 (h:t) = q36Ac [h] t [] 
    where q36Ac :: Ord a => [a] -> [a] -> [a] -> [a] 
          q36Ac l1 [] ac = ac++l1
          q36Ac l1 (h:t) ac 
            | (head l1) <= h = q36Ac [h] t (ac++l1)
            | otherwise = ac++l1

q36' :: Ord a => [a] -> [a]
q36' [] = []
q36' [x] = [x] 
q36' (h:s:t) 
    | s >= h = h : q36' (s:t)
    | otherwise = [h]

-- q37 :: Ord a => [a] -> [a]
-- q37 [] = [] 
-- q37 [x] = [x]
-- q37 (h:h1:t) 
--     | h <= h1 =  h : q37 (h1:t) 
--     | otherwise = q37 (h1:t)++[h]

q37' :: Ord a => [a] -> [a]
q37' [] = [] 
q37' (x:xs) = q37aux x (q37' xs)
    where q37aux :: Ord a => a -> [a] -> [a] 
          q37aux x [] = [x]
          q37aux x (y:ys) 
            | x <= y = x:y:ys 
            | otherwise = y:(q37aux x ys)

q38 :: String -> String -> Bool 
q38 [] _ = True 
q38 _ [] = False 
q38 (x:xs) (y:ys) 
    | ord x <= ord y = q38 xs ys 
    | otherwise = False

q39 :: Eq a => a -> [(a, Int)] -> Bool
q39 x [] = False 
q39 x ((h,h1):t) 
    | x == h = True
    | otherwise = q39 x t 

q40 :: [(a, Int)] -> [a]
q40 [] = []
q40 ((h,1):t) = h:(q40 t)
q40 ((h,n):t) = h:(q40 ((h, n-1):t))

q41 :: Eq a => a -> [(a, Int)] -> [(a, Int)]
q41 x [] = [(x,1)]
q41 x ((h, h1):t) 
    | x == h = ((h, h1+1):t)
    | otherwise = (h, h1) : q41 x t 

q42 :: Eq a => a -> [(a, Int)] -> [(a, Int)]
q42 x [] = []
q42 x ((h, h1):t) 
    | x == h = if h1 > 1 
               then (h, h1-1):t
               else t 
    | otherwise = (h, h1) : q42 x t 

q43 :: Ord a => [a] -> [(a, Int)]
q43 [] = []
q43 (x:xs) = reverse (q41 x (q43 xs))

q44 :: [Either a b] -> ([a], [b])
q44 [] = ([], []) 
q44 (h:t) = 
    case h of
        Left a -> (a:x, y)
        Right b -> (x, b:y)
    where (x,y) = q44 t 

q45 :: [Maybe a] -> [a]
q45 [] = []
q45 (h:t) = 
    case h of 
        Nothing -> q45 t
        Just x -> x : q45 t

data Movimento'' = Norte | Sul | Este | Oeste
    deriving Show

q46 :: (Int, Int) -> (Int, Int) -> [Movimento'']
q46 (x,y) (x2,y2) 
    | y < y2 = Este : q46 (x,y+1) (x2,y2)
    | y > y2 = Oeste : q46 (x,y-1) (x2,y2)
    | x < x2 = Norte : q46 (x+1,y) (x2,y2)
    | x > x2 = Sul : q46 (x-1,y) (x2,y2)
    | otherwise = []

q47 :: (Int, Int) -> [Movimento''] -> Bool 
q47 pos mov = q47Ac pos mov pos
    where q47Ac :: (Int, Int) -> [Movimento''] -> (Int, Int) -> Bool
          q47Ac _ [] _ = False
          q47Ac (x,y) (h:t) (x2,y2) = 
              case h of 
                Este -> q47Ac (x,y) t (x2+1,y2) || x == x2+1 && y == y2
                Oeste -> q47Ac (x,y) t (x2-1, y2) || x == x2-1 && y == y2 
                Norte -> q47Ac (x,y) t (x2, y2+1) || x == x2 && y == y2+1
                Sul -> q47Ac (x,y) t (x2, y2-1) || x == x2 && y == y2-1
            
type Ponto' = (Float,Float)
data Rectangulo = Rect Ponto' Ponto'

q48 :: [Rectangulo] -> Int
q48 [] = 0
q48 (Rect (x,y) (x2,y2):t) 
    | modulo (x-x2) == modulo (y-y2) = 1 + q48 t
    | otherwise = q48 t 

modulo :: Float -> Float
modulo x 
    | x >= 0 = x 
    | otherwise = x * (-1)

q49 :: [Rectangulo] -> Float
q49 [] = 0 
q49 (Rect (x,y) (x2,y2):t) = (modulo (x-x2) * modulo (y-y2)) + q49 t 

data Equipamento = Bom | Razoavel | Avariado
    deriving Show

q50 :: [Equipamento] -> Int 
q50 [] = 0 
q50 (h:t) = 
    case h of 
        Bom -> 1 + q50 t 
        Razoavel -> 1 + q50 t 
        Avariado -> q50 t


-- ---------------------------------------------------------------
-- ---------------------------------------------------------------
-- ---------------------------------------------------------------

qq9 :: Int -> a -> [a]
qq9 0 x = []
qq9 n x = x : qq9 (n-1) x 

qq36 :: Ord a => [a] -> [a] 
qq36 [] = []
qq36 (x:y:t)
    | x <= y = x : qq36 (y:t)
    | otherwise = [x]

qq47 :: (Int, Int) -> [Movimento''] -> Bool
qq47 pos mov = qq47Ac pos mov pos
    where qq47Ac :: (Int,Int) -> [Movimento''] -> (Int, Int) -> Bool
          qq47Ac _ [] _ = False 
          qq47Ac (x,y) (h:t) (x2,y2) = 
              case h of 
                Este -> qq47Ac (x,y) t (x2+1, y2) || x == x2+1 && y == y2
                Oeste -> qq47Ac (x,y) t (x2-1, y2) || x == x2-1 && y == y2
                Norte -> qq47Ac (x,y) t (x2, y2+1) || x == x2 && y == y2+1
                Sul -> qq47Ac (x,y) t (x2, y2-1) || x == x2 && y == y2-1

qq6 :: Int -> [a] -> [a]
qq6 _ [] = []
qq6 n (h:t) 
    | n > 0 = h : qq6 (n-1) t 
    | otherwise = []

qq7 :: Int -> [a] -> [a]
qq7 _ [] = []
qq7 n (h:t)
    | n > 0 = qq7 (n-1) t 
    | otherwise = h:t

qq31 :: Ord a => a -> [a] -> [a]
qq31 x [] = [x]
qq31 x (h:t) 
    | x <= h = x:h:t
    | otherwise = h : (qq31 x t)

qq11 :: Eq a => [a] -> [[a]]
qq11 [] = [[]]
qq11 (h:t) = q11Ac [h] t 
    where qq11Ac :: Eq a => [a] -> [a] -> [[a]] 
          qq11Ac h (h1:t) 
            | head h == h1 = qq11Ac (h1:h) t 
            | otherwise = h : qq11Ac [h1] t 

qq43 :: Ord a => [a] -> [(a, Int)]
qq43 [] = []
qq43 (h:t) = reverse (adiciona h (qq43 t))
    where adiciona :: Ord a => a -> [(a, Int)] -> [(a, Int)]
          adiciona x [] = [(x, 1)]
          adiciona x ((h, h1):t)
            | x == h = (h, h1+1):t
            | otherwise = (h,h1) : adiciona x t 

qq24 :: Eq a => [a] -> [a] -> Bool 
qq24 [] _ = True
qq24 _ [] = False 
qq24 (h:t) (h1:t1) = h == h1
                     && qq24 t t1 
                     || qq24 (h:t) t1

-- isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
-- isSubsequenceOf' [] _ = True 
-- isSubsequenceOf' _ [] = False
-- isSubsequenceOf' [h] (x:y) = h == x || isSubsequenceOf' [h] y
-- isSubsequenceOf' (h:t:t1)(x:y)
--     |h == x = isSubsequenceOf' (t:t1) y
--     |h /= x = isSubsequenceOf' (h:t:t1) y 
--     |otherwise = False

isSubsequenceOf'' :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOf'' [] _ = True
isSubsequenceOf'' _ [] = False 
isSubsequenceOf'' (x:xs) y
    | x==head y = isSubsequenceOf'' xs (tail y) 
    | otherwise = isSubsequenceOf'' (x:xs) (tail y)


qqq24 :: Eq a => [a] -> [a] -> Bool
qqq24 [] _ = True 
qqq24 _ [] = False 
qqq24 (x:xs) (y:ys)
    | x == y = qqq24 xs ys 
    | otherwise = qqq24 (x:xs) ys 

qq27 :: Eq a => a -> [a] -> [a] 
qq27 x [] = []
qq27 x (h:t) 
    | x == h = t 
    | otherwise = h : qq27 x t 

qq2 :: Int -> Int -> Int -> [Int] 
qq2 x y z 
    | x <= y && x <= z || x >= y && x >= z = x : qq2 y (y+(y-x)) z 
    | otherwise = []

enumFromThenTo''' :: Int -> Int -> Int -> [Int]
enumFromThenTo''' start next end
    | start > end && next > start || start < end && next < start || start == next && start > end = []
    | otherwise = start : enumFromThenTo''' next (2 * next - start) end

qq41 :: Eq a => a -> [(a, Int)] -> [(a, Int)]
qq41 x [] = [(x,1)]
qq41 x ((h,h1):t) 
    | x == h = ((h,h1+1)):t 
    | otherwise = (h,h1) : qq41 x t 

qq15 :: [[a]] -> [a]
qq15 [] = []
qq15 ([]:t) = qq15 t
qq15 ((h:t):t1) = h : qq15 t1 

qq29 :: Eq a => [a] -> [a] -> [a]
qq29 l1 l2 = qq29Ac l1 l2 []
    where qq29Ac :: Eq a => [a] -> [a] -> [a] -> [a] 
          qq29Ac [] (y:ys) ac = qq29Ac (ac++[y]) ys []
          qq29Ac (x:xs) [] ac = ac++(x:xs)
          qq29Ac (x:xs) (y:ys) ac 
            | x == y = qq29Ac (ac++xs) ys []
            | otherwise = qq29Ac xs (y:ys) (ac++[x])

qqq29 :: Eq a => [a] -> [a] -> [a]
qqq29 _ [] = [] 
qqq29 (x:xs) (y:ys) 
    | y `elem` (x:xs) = qqq29 (x:xs) ys  
    | otherwise = qqq29 (x:xs) ys ++ [y]

qq17 :: [(a,b,c)] -> [(a,c)]
qq17 [] = []
qq17 ((a,b,c):t) = (a,c):(qq17 t)

qq18 :: [(String,b,c)] -> String
qq18 [] = []
qq18 ((a,b,c):t) = (a ++ qq18 t)

qq42 :: Eq a => a -> [(a,Int)] -> [(a,Int)]
qq42 x [] = []
qq42 x ((a,b):t) 
    | x == a && b > 1 = (a,b-1):qq42 x t 
    | x == a && b == 1 = qq42 x t 
    | otherwise = (a,b):qq42 x t 

qq37 :: Ord a => [a] -> [a]
qq37 [] = []
qq37 (h:t) = insere' h (qq37 t)

insere' :: Ord a => a -> [a] -> [a]
insere' x [] = [x]
insere' x (h:t)
    | x <= h = x:h:t 
    | otherwise = h : insere' x t 

qq39 :: Eq a => a -> [(a,Int)] -> Bool
qq39 x [] = False 
qq39 x ((a,_):t) 
    | x == a = True 
    | otherwise = qq39 x t 

qq8 :: [a] -> [b] -> [(a,b)]
qq8 [] _ = []
qq8 _ [] = []
qq8 (x:xs) (y:ys) = (x,y) : qq8 xs ys 

qq28 :: Eq a => [a] -> [a] -> [a] 
qq28 l [] = l 
qq28 [] _ = [] 
qq28 l1 (x:xs) = qq28 (remove x l1) xs 
    -- | y `elem` (x:xs) = (remove y (x:xs) ++ qq28 (x:xs) ys)
    -- | otherwise = qq28 (x:xs) ys
    where remove :: Eq a => a -> [a] -> [a]
          remove x [] = []
          remove x (h:t)
            | x == h = t 
            | otherwise = x : remove x t 

qq33 :: [String] -> String
qq33 [] = []
qq33 (h:t) = h ++ "\n" ++ qq33 t 

qq19 :: Int -> Int -> [(String,Int)] -> [String] 
qq19 _ _ [] = []
qq19 ano idade ((nome, nascimento):t) 
    | ano - nascimento >= idade = (nome : qq19 ano idade t)
    | otherwise = qq19 ano idade t

qq20 :: Int -> Int -> [Int]
qq20 n m = qq20Ac n m 0 
    where qq20Ac :: Int -> Int -> Int -> [Int]
          qq20Ac n m ac
            | ac <= m - 1  = (n^ac) : qq20Ac n m (ac+1)
            | otherwise = [] 

qq44 :: [Either a b] -> ([a], [b])
qq44 [] = ([],[])
qq44 (h:t) = 
    case h of
        Left a -> (a:x, y)
        Right b -> (x, b:y)
    where (x,y) = qq44 t 

qqq15 :: [[a]] -> [a] 
qqq15 [] = []
qqq15 ([]:t) = qqq15 t 
qqq15 ((h:t):t1) = h : qqq15 t1 

-- qq34 :: Ord a => [a] -> Int 
-- qq34 l = qq34Ac l 0 0 
--     where qq34Ac :: Ord a => [a] -> Int -> Int 
--           qq34Ac [] ac _ = ac
--           qq34Ac (h:h1:t) ac 
--             | h > h1 = qq34Ac (h1:t) (ac+ac2+1) ac2
--             | otherwise = qq34Ac (h1:t) ac (ac2+1)

pMaior :: Ord a => [a] -> Int
pMaior [_] = 0
pMaior l@(x:xs)
        | x == pMaior' l = 0
        | otherwise = 1 + pMaior xs
-- compara o primeiro elemento da lista com o maior numero 

pMaior' :: Ord a => [a] -> a
pMaior' [x] = x
pMaior' (x:y:zs)
        | x >= y = pMaior'(x:zs)
        | otherwise = pMaior'(y:zs)
-- verifica qual é o maior numero 

qq11' :: Eq a => [a] -> [[a]] 
qq11' (h:t) = qq11'Ac [h] t 
    where qq11'Ac :: Eq a => [a] -> [a] -> [[a]]
          qq11'Ac h [] = [h]
          qq11'Ac h (x:xs) 
            | (head h) == x = qq11'Ac (x:h) xs 
            | otherwise = h : qq11'Ac [x] xs 

qqq20 :: Int -> Int -> [Int]
qqq20 n m = qqq20Ac n m 0 
    where qqq20Ac :: Int -> Int -> Int -> [Int]
          qqq20Ac n m ac 
            | m -1 >= ac = (n^ac) : qqq20Ac n m (ac+1)
            | otherwise = []

qq11'' :: Eq a => [a] -> [[a]] 
qq11'' (h:t) = qq11''Ac [h] t 
    where qq11''Ac :: Eq a => [a] -> [a] -> [[a]]
          qq11''Ac h [] = [h]
          qq11''Ac h (h1:t)
            | head h == h1 = qq11''Ac (h1:h) t 
            | otherwise = h : qq11''Ac [h1] t 

qqqq24 :: Eq a => [a] -> [a] -> Bool 
qqqq24 [] _ = True
qqqq24 _ [] = False
qqqq24 (x:xs) (y:ys) 
    | y == x = qqqq24 xs ys 
    | otherwise = qqqq24 xs (y:ys)

qq12 :: [[a]] -> [a]
qq12 [] = []
qq12 ([]:t) = qq12 t 
qq12 ((h:t):t1) = h : qq12 (t:t1)

qq13 :: [a] -> [[a]]
qq13 [] = [[]]
qq13 t = t : qq13 (tail t)
auxx :: [a] -> [[a]]
auxx t = reverse (qq13 t) 

qq5 :: [a] -> [a] 
qq5 l = qq5Ac l []
    where qq5Ac :: [a] -> [a] -> [a]
          qq5Ac [] ac = ac 
          qq5Ac (h:t) ac = qq5Ac t (h:ac)

qq44' :: [Either a b] -> ([a], [b])
qq44' [] = ([], [])
qq44' (h:t) = 
    case h of
        Left a -> (a:x, y)
        Right b -> (x, b:y)
    where (x,y) = qq44' t 

qqq27 :: Eq a => a -> [a] -> [a] 
qqq27 x [] = []
qqq27 x (h:t)
    | x == h = t 
    | otherwise = h : qqq27 x t 

isPrime :: Int -> Bool
isPrime n
    | n >= 2 = primeCheck n 2
    | otherwise = False

primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = True
    | mod n m == 0 = False 
    | otherwise = primeCheck n (m+1) 

q1 :: Int -> Int -> [Int] 
q1 x y 
    | y - x >= 0 = x : q1 (x+1) y 
    | otherwise = [] 



enumFromTo''' :: Int -> Int -> [Int]
enumFromTo''' start end 
    | start > end = []
    | otherwise = start : enumFromTo''' (start + 1) end

-- ------------------------------------------------------


q7 :: Int -> [a] -> [a]
q7 0 l = l 
q7 _ [] = []
q7 x (h:t) = q7 (x-1) t 

-- maxSumInit :: (Num a, Ord a) => [a] -> a
-- maxSumInit l = maximum [sum m | m <- inits l]

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maxSI l 0 0 
    where maxSI :: (Num a, Ord a) => [a] -> a -> a -> a
          maxSI [] m s = m 
          maxSI (x:xs) m s 
            | s+x > m = maxSI xs (s+x) (s+x)
            | otherwise = maxSI xs m (s+x)

qq10 :: a -> [a] -> [a]
qq10 _ [] = []
qq10 _ [x] = [x]
qq10 x (h:t) = h:x:(qq10 x t)

seqFib :: [Integer]
seqFib = 0 : 1 : [ a+b | (a,b) <- zip seqFib (tail seqFib)]
-- exemplo de utilizaçao -> take 2 seqFib = [1,1]

q3 :: [a] -> [a] -> [a]
q3 [] l = l 
q3 (h:t) l =  h : q3 t l 

q4 :: [a] -> Int -> a 
--q4 (h:_) 0 = h 
q4 (h:t) x 
    | x == 0 = h 
    | otherwise =  q4 t (x-1)

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' l l' = last l == last l' 
                   && isSuffixOf' (init l) (init l')

-- q23 :: Eq a => [a] -> [a] -> Bool 
-- q23 [] _ = True 
-- q23 _ [] = False 
-- q23  

qqq5 :: [a] -> [a]
qqq5 l = qq5' l [] 
    where qq5' :: [a] -> [a] -> [a]
          qq5' [] ac = ac 
          qq5' (h:t) ac = qq5' t (h:ac)


