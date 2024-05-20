--Máquina de Somar
maquinaSomar :: [Int] -> [Int]
maquinaSomar result = soma result 0 True True
    where
        soma [] somado firstZero realizouSoma
            | somado == 0 && realizouSoma == True = []
            | otherwise = [somado]
        soma (x:xs) somado firstZero realizouSoma
            | x == 0 && xs == [] && somado == 0 && realizouSoma == True = []
            | x == 0 && realizouSoma == False = somado : soma xs 0 False True
            | x == 0 && somado == 0 && firstZero == True = soma xs 0 False realizouSoma --achou o primeiro zero
            | x == 0 && somado == 0 && firstZero == False = [] --achou o segundo zero
            | otherwise = soma xs (somado + x) True False

main = do
       lista <- getLine
       print $ maquinaSomar (read lista :: [Int])
