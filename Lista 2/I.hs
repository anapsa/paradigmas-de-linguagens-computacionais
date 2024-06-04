--Suavização de Dados
suaviza :: [Float] -> [Float]
suaviza x = aux x True 0 0 --haskell
    where 
        aux [] _ x y = [] 
        aux(x:[]) True somar y = x : aux [] False x y
        aux(x:[]) False somar y = y : aux [] False x y
        aux (x:y:xs) True _ _ = x : aux (y:xs) False x y
        aux (x:y:xs) _ somar _ = (somar+x+y)/3 : aux (y:xs) False x y

main = do
        lista <- getLine
        print $ suaviza (read lista :: [Float])
