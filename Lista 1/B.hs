maquinaSomar :: [Int] -> [Int]
maquinaSomar result = soma result 0 True
    where
        soma [] somado _ = [somado] 
        soma (x:xs) somado firstZero
            | x == 0 && somado == 0 && firstZero == True = soma xs 0 False
            | x == 0 && somado == 0 && firstZero == False = []
            | x == 0 = somado : soma xs 0 False
            | otherwise = soma xs (somado + x) False

main = do
       lista <- getLine
       print $ maquinaSomar (read lista :: [Int])
