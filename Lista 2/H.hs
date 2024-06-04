--Calculadora
type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa x = aux x 0 
    where 
        aux [] operador = operador
        aux ((x,y):xs) operador
            | x == "Soma" = aux xs (operador + y)
            | x == "Subtrai" = aux xs (operador - y)
            | x == "Multiplica" = aux xs (operador * y)
            | x == "Divide" && y == 0 = aux [] (-666)
            | x == "Divide" = aux xs (operador `div` y)
            | otherwise = aux xs operador

main = do
    a <- getLine
    let result = executa (read a :: [(Comando, Valor)])
    print result
