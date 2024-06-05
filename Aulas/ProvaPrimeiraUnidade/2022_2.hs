-- 2) (4.0) Escreva duas funções que que fazem esse mesmo processo para
--qualquer sequência de letras. Assuma que a lista não possui números, apenas
--letras, e que no máximo as letras se repetem 9 vezes. Se a letra não é seguida
--por um número é porque ela não se repete naquele momento. Por exemplo:

--essa função é válida porque as letras podem ser repetir só 9 vezes
toChar :: Int -> Char 
toChar 1 = '1'
toChar 2 = '2'
toChar 3 = '3'
toChar 4 = '4' 
toChar 5 = '5' 
toChar 6 = '6'
toChar 7 = '7' 
toChar 8 = '8' 
toChar 9 = '9'

contaLetras :: String -> Char -> Int -> String 
contaLetras [] _ 0 = [] 
contaLetras [] letra contador = [letra,(toChar contador)]
contaLetras (x:xs) letra contador 
    | letra == ' ' = x : contaLetras xs x 0
    | x == letra = contaLetras xs x (contador+1) 
    | contador /= 0 = [(toChar (contador+1))] ++ [x] ++ contaLetras xs x 0
    | otherwise = x : contaLetras xs x 0 

rlencodeLetras :: String -> String
rlencodeLetras x = contaLetras x ' ' 0 

replicaLetras :: String -> Char -> String 
replicaLetras [] _ = []
replicaLetras (x:xs) letra 
    | x >= '1' && x <= '9' = (replicate (fromEnum(x)-49) letra) ++ replicaLetras xs x -- o menos 49 é porque o fromEnum devolve o valor da tabela ascii do caractere e o valor do 0 é 48, e é subtraido 1 de novo porque ja foi concatenado letra uma vez 
    | otherwise = x : replicaLetras xs x 
rldecodeLetras :: String -> String
rldecodeLetras x = replicaLetras x ' '
 
-- 3) (2.0) Dado um tipo de dados que representa letras únicas ou letras repetidas:

data Letra = Unica Char | Repetida Char Int
    deriving Show

contaLetras1 :: String -> Char -> Int -> [Letra] 
contaLetras1 [] letra 0 = [(Unica letra)]
contaLetras1 [] letra contador = [(Repetida letra contador)]
contaLetras1 (x:xs) letra contador 
    | letra == ' ' = contaLetras1 xs x 0 
    | x == letra = contaLetras1 xs x (contador+1) 
    | contador /= 0 = [(Repetida letra (contador+1))] ++ contaLetras1 xs x 0 
    | otherwise = [(Unica letra)] ++ contaLetras1 xs x 0 
    
rlencodeLetrasCodigo :: String -> [Letra]
rlencodeLetrasCodigo x = contaLetras1 x ' ' 0

rldecodeLetrasCodigo :: [Letra] -> String
rldecodeLetrasCodigo x = replicaLetras1 x 

replicaLetras1 :: [Letra] -> String 
replicaLetras1 [] = []
replicaLetras1 ((Unica x):xs) = x : replicaLetras1 xs 
replicaLetras1 ((Repetida x num):xs) = (replicate num x) ++ replicaLetras1 xs

