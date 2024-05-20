--Fatura do Cartão
eMes :: String -> Bool 
eMes "JAN" = True
eMes "FEV" = True
eMes "MAR" = True
eMes "ABR" = True
eMes "MAI" = True
eMes "JUN" = True
eMes "JUL" = True
eMes "AGO" = True
eMes "SET" = True
eMes "OUT" = True
eMes "NOV" = True
eMes "DEZ" = True
eMes _ = False 

eNumero :: String -> Bool 
eNumero "" = False
eNumero (str:strs)  
    | str == '.' = True 
    | otherwise = eNumero strs 

novaLista :: [String] -> [String]
novaLista strs = [str | str <- strs, eNumero str || eMes str]

soma :: String -> [String] -> Double -> Bool -> Double
soma _ [] total _ = total 
soma mes (x:xs) total mesIgual
    | x == mes = soma mes xs (total) True 
    | eNumero(x) && mesIgual = soma mes xs (total + read x :: Double) False
    | otherwise = soma mes xs total False 

    
logMes :: String -> String -> Double
logMes mes resultado = soma mes (novaLista (separa resultado )) 0 False 
    where
        separa "" = [""]
        separa (x:xs)
            | x == ';' || x == ' ' = "" : resto
            | otherwise = (x : head resto) : tail resto
            where
                resto = separa xs
        
       

main :: IO ()
main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result


