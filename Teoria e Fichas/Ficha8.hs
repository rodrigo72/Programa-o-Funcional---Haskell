-- Resolução da ficha 8 de Programação Funcional 
-- Classes de Tipos
module Ficha8 where

{-
1. a)

Defina a função normaliza :: Frac -> Frac que dada uma fracção calcula uma fracção equivalente, irredutível, 
e com o denominador positivo. Por exemplo, normaliza (F (-33) (-51)) deve retornar F 11 17 e normaliza (F 50 (-5)) 
deve retornar F (-10) 1. Sugere-se que comece por definir primeiro a função mdc :: Integer -> Integer -> Integer 
que calcula o máximo divisor comum entre dois números, baseada na seguinte propriedade (atribuida a Euclides): 
mdc x y == mdc (x+y) y == mdc x (y+x)

Imagem: 2021-12-24-14-42-45.png
-}

data Frac = F Integer Integer 

mdc :: Integer -> Integer -> Integer
mdc x y = maximum (comumListas (divisores x) (divisores y))

comumListas [] _ = []
comumListas _ [] = []
comumListas (s:s1) s2 = 
    if elem s s2 then s : comumListas s1 s2
                 else comumListas s1 s2

divisores x = [a | a <- [1..x], x `mod` a == 0]

-- Maneira mais eficiente: 
mdc' :: Integer -> Integer -> Integer
mdc' a b = last [n | n <- [1..(min a b)] , a `mod` n == 0, b `mod` n == 0]

normaliza :: Frac -> Frac
normaliza (F a b) = F ((abs a) `div` x) (oh `div` x)
    where x = mdc (abs a) (abs b) * (if a * b < 0 then (-1) else 1)
          oh | a < 0 = -b 
             | otherwise = b 

-- abs é o absolute value, ou seja, abs (-1) = 1, abs 1 = 1 -- é a norma/ módulo 

{-
1. b)

Defina Frac como instância da classe Eq.
-}

instance Eq Frac where 
    (F a b) == (F c d) = a * d == b * c  

{-
1. c)

Defina Frac como instˆancia da classe Ord
-}

instance Ord Frac where 
    (F a b) <= (F c d) = a * d <= c * b 

{-
1. d)

Defina Frac como instˆancia da classe Show, de forma a que cada frac ̧c ̃ao seja
apresentada por (numerador/denominador).
-}

instance Show Frac where 
    show (F a b) = show a ++ " / " ++ show b 

{-
1. e)

Defina Frac como instˆancia da classe Num. Relembre que a classe Num tem a
seguinte defini ̧c ̃ao
-}

instance Num Frac where 
    (F a b) + (F c d) | b == d = normaliza $ F (a + c) b
                      | otherwise = normaliza $ F (a * d + b * c) (b * d)
    x - y = x + negate y
    (F a b) * (F c d) = F (a * c) (b * d)
    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) | a == 0 = 0
                   | a * b > 0 = 1
                   | otherwise = -1
    fromInteger x = F x 1

{-
2. a)

Relembre o tipo definido na Ficha 7 para representar express ̃oes inteiras. Uma poss ́ıvel
generaliza ̧c ̃ao desse tipo de dados, ser ́a considerar express ̃oes cujas constantes s ̃ao de
um qualquer tipo num ́erico (i.e., da classe Num)

Declare Exp a como uma instˆancia de Show
-}

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

-- Só podemos declarar (Exp a) como instância da classe Show se o tipo a for também uma instância da classe Eq. 
-- Este tipo de restrição pode ser colocado na declaraçao de instância, fazendo:
instance Show a => Show (Exp a) where   
    show (Const a) = show a 
    show (Simetrico a) = "(-" ++ show a ++ ")"
    show (Mais a b) = show a ++ " + " ++ show b 
    show (Menos a b) = show a ++ " - " ++ show b 
    show (Mult a b) = show a ++ " * " ++ show b 
-- Ou então, esta declaração de instância poderia ser derivada automaticamente colocando "deriving (Show)" depois do data type ser definido.

{-
2. b)

Declare Exp a como uma intância de Eq
-}

-- instance (Num a, Eq a) => Eq (Exp a) where 
--     Const a == Const b = a == b 
--     Simetrico a == Simetrico b = -(a) == -(b) 
--     Mais a b == Mais c d = a + b == c + d 
--     Menos a b == Menos c d = a - b == c - d 
--     Mult a b == Mult c d = a * b == c * d 

valor :: (Num a) => Exp a -> a
valor (Const a) = a
valor (Simetrico a) = - (valor a)
valor (Mais a b) = valor a + valor b
valor (Menos a b) = valor a - valor b
valor (Mult a b) = valor a * valor b

instance (Num a,Eq a) => Eq (Exp a) where
    x == y = valor x == valor y

{-
2. c)

Declare Exp a commo uma instãncia da classe Num
-}

instance (Num a, Eq a) => Num (Exp a) where 
    x + y = Const (valor x + valor y)
    x - y = Const (valor x - valor y)
    x * y = Const (valor x * valor y)
    negate (Const a) = Const (-a)
    negate (Simetrico a) = a 
    negate (Mais a b) = Mais (-a) (-b)
    negate (Menos a b) = Menos b a 
    negate (Mult a b) = Mult (-a) b 
    fromInteger x = Const (fromInteger x)
    abs (Const a) = Const (abs a)
    abs (Simetrico a) = abs a 
    abs (Mais a b) = abs (a + b)
    abs (Menos a b) = abs (a - b) 
    abs (Mult a b) = abs (a * b)
    signum (Const a) = if abs a == a then if a == 0 then 0 else 1 else (-1)
    signum (Simetrico a) = - signum a 
    signum (Mais a b) = Const (if abs (a + b) == a + b then if a + b == 0 then 0 else 1 else (-1))
    signum (Menos a b) = Const (if abs (a - b) == a - b then if a - b == 0 then 0 else 1 else (-1))
    signum (Mult a b) = Const (if abs (a * b) == a * b then if a * b == 0 then 0 else 1 else (-1))


{-
3. a)

Relembre o exerc ́ıcio da Ficha 3 sobre contas banc ́arias, com a seguinte declara ̧c ̃ao de
tipos

Defina Data como instˆancia da classe Ord.
-}

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

instance Eq Data where
    (D a b c) == (D d e f) = a*b*c == d*e*f 

instance Ord Data where 

    -- Para declarar um tipo como instância da classe Ord, basta definir a função (<=) ou a função compare
    -- A função compare pode ser mais eficiente para tipos mais complexos 
    -- compare devolte ou GT ou EQ ou LT, como se pode ver a baixo 

    (D ano1 mes1 dia1) <= (D ano2 mes2 dia2) = ano1 <= ano2 || ano1 == ano2 && (mes1 <= mes2 || mes1 == mes2 && dia1 <= dia2)

    compare (D dia1 mes1 ano1) (D dia2 mes2 ano2) 
        | ano1 > ano2 || ano1 == ano2 && (mes1 > mes2 || mes1 == mes2 && dia1 > dia2) = GT
        | ano1 == ano2 && mes1 == mes2 && dia1 == dia2 = EQ 
        | otherwise = LT 

{-
3. b)

Defina Data como instˆancia da classe Show.
-}

instance Show Data where 
    show (D ano mes dia) = show ano ++ "/" ++ show mes ++ "/" ++ show dia

{-
3. c)

Defina a fun ̧c ̃ao ordena :: Extracto -> Extracto, que transforma um ex-
tracto de modo a que a lista de movimentos apare ̧ca ordenada por ordem crescente
de data.
-}

-- data Extracto = Ext Float [(Data, String, Movimento)]

ordena :: Extracto -> Extracto 
ordena (Ext x l) = (Ext x (ordena2 l))

ordena2 :: [(Data, String, Movimento)] -> [(Data, String, Movimento)]
ordena2 [] = [] 
ordena2 (h:t) = insere h (ordena2 t) 

insere :: (Data, String, Movimento) -> [(Data, String, Movimento)] -> [(Data, String, Movimento)]
insere x [] = [x]
insere x1@(data1,_,_) (x2@(data2,_,_):t) = 
    if data1 <= data2 
    then x1:x2:t
    else x2 : insere x1 t 

-- insere :: (Data, String, Movimento) -> Extracto -> Extracto
-- insere data (Ext x []) = (Ext x [data])
-- insere x1@(data1,s1,m1) (Ext x (x2@(data2,s2,m2):t)) = 
--     if data1 <= data2
--     then (Ext x (x1:x2:t))
--     else insere data2 t 

instance Show Movimento where 
    show (Credito a) = show a 
    show (Debito b) = "         " ++ show b 

instance Show Extracto where 
    show (Ext x l) = 
        "\n" ++ 
        "Saldo anterior: " ++ show x ++ "\n" ++
        produzTracinho (length belaString) ++ "\n" ++
        belaString ++ "\n" ++ 
        mostraDatas l (ordena2 l) ++ "\n" ++
        produzTracinho (length belaString) ++ "\n" ++ 
        "Saldo atual: " ++ show (saldoAtual x l)  ++ "\n" 

        where belaString :: String 
              belaString = "Data         Descricao" ++ produzEspacos (if ((maiorString l) - 9) <= 0 then 0 else ((maiorString l) - 9)) ++ "   " ++ "Credito  Debito"


mostraDatas :: [(Data, String, Movimento)] -> [(Data, String, Movimento)] -> String 
mostraDatas _ [] = ""
mostraDatas l [(data1, string, movimento)] = show data1 ++ "   " ++ show string ++ "   " ++ show movimento 
mostraDatas l ((data1, string, movimento):t) = 
    show data1 ++ "   " ++ show string ++ produzEspacos (if (maiorString l) - 2 <= 9 then (9 - (length string)) - 2 else maiorString l - (length string) -  2) ++ "   " ++ show movimento ++ "\n" ++ mostraDatas l t 



maiorString :: [(Data, String, Movimento)] -> Int
maiorString [(_,string,_)] = length string 
maiorString ((a,string,b):(c,string2,d):t)
    | length string >= length string2 = maiorString ((a,string,b):t) 
    | otherwise = maiorString ((c,string2,d):t) 

produzEspacos :: Int -> String 
produzEspacos 0 = ""
produzEspacos n = " " ++ produzEspacos (n-1) 

produzTracinho :: Int -> String 
produzTracinho 0 = ""
produzTracinho n = "-" ++ produzTracinho (n-1) 


saldoAtual :: Float -> [(Data, String, Movimento)] -> Float 
saldoAtual x [] = x
saldoAtual x ((_,_, Credito y):t) = y + saldoAtual x t 
saldoAtual x ((_,_, Debito  y):t) = -y + saldoAtual x t 

-- fartei-me 
-- adios 
-- fim da ficha 




