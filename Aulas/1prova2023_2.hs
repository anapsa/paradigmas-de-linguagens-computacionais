--segunda questão
merge :: Ord t => [t] -> [t] -> [t]
merge [] [] = [] 
merge [] ys = ys 
merge xs [] = xs 
merge (x:xs) (y:ys) 
    | y > x = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
--terceira questão 
type Pilha t = [t]
data Elemento = Valor Int | Soma | Multiplica deriving (Show) 

--exemploPilhaElem :: Pilha Elemento
--exemploPilhaElem = [Valor 10, Valor 20, Soma, Valor 30, Multiplica]
gera_string :: Pilha Elemento -> String
gera_string pilha = aux pilha ""
    where 
        aux [] str = str 
        aux((Valor a):(Valor b):Soma:xs) str = aux xs ( "(" ++ show(a) ++ "+" ++ show(b) ++ ")")
        aux((Valor a):(Valor b):Multiplica:xs) str =  aux xs ("(" ++ show(a) ++ "*" ++ show(b) ++ ")")
        aux((Valor a):Soma:xs) str = aux xs ("(" ++ str ++ "+" ++ show(a) ++ ")")
        aux((Valor a):Multiplica:xs) str = aux xs ("(" ++ str ++ "*" ++ show(a) ++ ")") 
        aux (x:xs) str =  aux xs str --esse caso nao existe

-- exemplo de uso: gera_string exemploPilhaElem ——> "((10+20)*30)"
