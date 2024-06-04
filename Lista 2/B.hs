--Palavras Frequentes 
count :: String -> [String] -> Int
count str [] = 1
count str (x:xs) 
    | str == x = 1 + count str xs 
    | otherwise = 0 + count str xs 

formaTupla :: [String] -> [(Int, String)] 
formaTupla [] = [] 
formaTupla (str:strs) = ((count str strs), str) : formaTupla (filter (\x -> x /= str) strs)

quickSort :: [(Int, String)] -> [(Int, String)] 
quickSort [] = []
quickSort (x:xs) = quickSort [a | a <- xs, fst a > fst x || (fst a == fst x && (length (snd a)) < (length (snd x)))] ++ [x] ++ quickSort [a | a <- xs, fst a < fst x || (fst a == fst x && (length (snd a)) > (length (snd x)))]             

transformaString :: [(Int, String)] -> [String] 
transformaString [] = [] 
transformaString (x:xs) = (snd x) : transformaString xs 

palavrasFrequentes :: [String] -> [String]
palavrasFrequentes list = transformaString (take 3 (quickSort (formaTupla list))) 

main = do
        lista <- getLine
        print $ palavrasFrequentes (read lista :: [String])
