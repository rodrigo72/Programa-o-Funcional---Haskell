-- Resolução da ficha 9 de Programação Funcional 
-- Input / Output 
module Ficha9 where

import System.Random 
import System.IO


-- Cálculo das raizes de um polinómio de segundo grau 

roots :: (Float,Float,Float) -> Maybe (Float,Float)
roots (a,b,c) 
    | d >= 0 = Just ((-b + (sqrt d))/(2*a), (-b - (sqrt d))/(2*a))
    | d < 0  = Nothing
  where d = b^2 - 4*a*c

calcRoots :: IO ()
calcRoots = 
   do putStrLn "Calculo das raizes do polimomio a x^2 + b x + c"
      putStr "Indique o valor do coeficiente a: "
      a <- getLine
      putStr "Indique o valor do coeficiente b: "
      b <- getLine
      putStr "Indique o valor do coeficiente c: "
      c <- getLine
      case (roots (read a, read b, read c)) of
         Nothing        -> putStrLn "Não há raizes reais."
         (Just (r1,r2)) -> putStrLn ("As raizes são "++(show r1)++" e "++(show r2))

{-
Também é possível fazer de outra maneira, utilizando o readIO :: Read a => String -> IO a 

calcROOTS :: IO ()
calcROOTS = 
   do putStrLn "Calculo das raizes do polimomio a x^2 + b x + c"
      putStr "Indique o valor do ceoficiente a: "
      a <- getLine
      a1 <- readIO a
      putStr "Indique o valor do ceoficiente b: "
      b <- getLine
      b1 <- readIO b
      putStr "Indique o valor do ceoficiente c: "
      c <- getLine
      c1 <- readIO c
      case (roots  (a1,b1,c1)) of
         Nothing        -> putStrLn "Nao ha' raizes reais"
         (Just (r1,r2)) -> putStrLn ("As raizes sao "++(show r1)
                                              ++" e "++(show r2))
-}

-- Exemplo carregar e descarregar uma base de dados de notas em ficheiro.

type Notas = [(Integer, String, Int)] 

dialogo :: String -> IO String
dialogo s = do putStr s
               r <- getLine
               return r 

dialogo' :: String -> IO String
dialogo' s = (putStr s) >> (getLine >>= (\r -> return r))

leFich :: IO ()
leFich = do file <- dialogo "Qual o nome do ficheiro ? "
            s <- readFile file
            let l = map words (lines s)
            print (geraNotas l)

geraNotas :: [[String]] -> Notas
geraNotas ([x,y,z]:t) = (read x, y, read z):(geraNotas t)
geraNotas _ = []

escFich :: Notas -> IO ()
escFich notas = do file <- dialogo "Qual o nome do ficheiro ? "
                   writeFile file (geraStr notas)
geraStr :: Notas -> String
geraStr [] = ""
geraStr ((x,y,z):t) = (show x) ++ ('\t':y) ++ ('\t':(show z)) ++ "\n" ++ (geraStr t)

{-
1. ) 

A classe Random da biblioteca System.Random agrupa os tipos para os quais é possível gerar valores aleatórios. 
Algumas das funções declaradas nesta classe são:

    randomIO :: Random a => IO a que gera um valor aleatório do tipo a;
    randomRIO :: Random a => (a,a) -> IO a que gera um valor aleatório do tipo a dentro de uma determinada gama de valores.

Usando estas funções implemente os seguintes programas:
a) bingo :: IO () que sorteia os números para o jogo do bingo. Sempre que uma tecla é pressionada é apresentado um número 
aleatório entre 1 e 90. Obviamente, não podem ser apresentados números repetidos e o programa termina depois de gerados os 
90 números diferentes.
-}

bingo :: IO ()
bingo = do putStrLn "==== Bingo =====" 
           geraNumeros 90 [1..90]

geraNumeros :: Int -> [Int] -> IO ()
geraNumeros 0 [] = putStrLn "The end"
geraNumeros n l  = do putStrLn "clica em qualquer coisa"
                      getChar 
                      i <- randomRIO (0,n-1)
                      let (a, x:b) = splitAt i l 
                      print x 
                      geraNumeros (n-1) (a++b)




{-
2. a) 

Uma aposta do EuroMilh ̃oes corresponde `a escolha de 5 N ́umeros e 2 Estrelas. Os
N ́umeros s ̃ao inteiros entre 1 e 50. As Estrelas s ̃ao inteiros entre 1 e 9. Para modelar
uma aposta destas definiu-se o seguinte tipo de dados:

Defina a fun ̧c ̃ao valida :: Aposta -> Bool que testa se uma dada aposta  ́e
v ́alida (i.e. tem os 5 n ́umeros e 2 estrelas, dentro dos valores aceites e n ̃ao tem
repeti ̧c ̃oes).
-}

data Aposta = Ap [Int] (Int,Int)

valida :: Aposta -> Bool 
valida (Ap l@(a:b:c:d:e:[]) (f,g)) = 
   and [x `elem` [1..50] | x <- [a,b,c,d,e]] && f `elem` [1..9] && g `elem` [1..9] && f /= g && (verifica l)
   where verifica :: [Int] -> Bool 
         verifica l = case l of 
            [] -> True
            (x:xs) -> x `notElem` xs && verifica xs 

{-
2. b) 

Defina a fun ̧c ̃ao comuns :: Aposta -> Aposta -> (Int,Int) que dada uma aposta
e uma chave, calcula quantos n ́umeros e quantas estrelas existem em comum nas
duas apostas
-}


