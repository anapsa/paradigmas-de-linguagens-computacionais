minMaxCartao :: String -> (Double, Double)
minMaxCartao fatura = resultado
    where
        separa "" = [""] 
        separa (x:xs) 
            | x == ';' = "" : resto 
            | otherwise = (x : head resto) : tail resto
                where 
                    resto = separa xs 
                    
        eNumero "" = False
        eNumero str = all eDigitoOuPonto str
            where
                eDigitoOuPonto c = eDigito c || c == '.'
                eDigito c = c `elem` "0123456789"

        paraDouble strs = [read str | str <- strs, eNumero str]
        
        bSort [] = []
        bSort (x:xs) = bSort[a | a <- xs, a < x]  ++ [x] ++ bSort[a | a <- xs, a >= x] 

        minMax [] = error "Lista vazia"
        minMax xs = (head ordenado, last ordenado)
            where ordenado = bSort xs

        resultado = minMax (paraDouble (separa fatura))

main :: IO ()
main = do
    a <- getLine
    let result = minMaxCartao a
    print result

