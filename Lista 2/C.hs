--Uncommon Words
separa :: String -> [String]
separa "" = [""]
separa (x:xs)
    | x == ' '  = "" : separa xs
    | otherwise = (x : head resto) : tail resto
    where
        resto = separa xs
toMin :: String -> String
toMin [] = []
toMin (x:xs) 
    | (fromEnum x) >= 65 && (fromEnum x) <= 90 = toEnum (fromEnum x + 32) : toMin xs 
    | otherwise = x : toMin xs
uncommonFromTwoSentences :: String -> String -> [String]
uncommonFromTwoSentences str1 str2 = bSort (compara (separa (toMin str1)) (separa (toMin str2)) ++ compara (separa (toMin str2)) (separa (toMin str1)))
    where 
        compara [] _ = [] 
        compara (x:xs) ys 
          | x `elem` ys = compara (filter (\k -> k /= x) xs) (filter (\k -> k /= x) ys)
          | x `elem` xs = compara (filter (\k -> k /= x) xs) (filter (\k -> k /= x) ys)
          | otherwise = x : compara xs ys
        
        bSort [] = []
        bSort (x:xs) = bSort[a | a <- xs, a < x]  ++ [x] ++ bSort[a | a <- xs, a >= x] 

            
    
main :: IO ()
main = do
    sentence_1 <- getLine
    sentence_2 <- getLine
    let result = uncommonFromTwoSentences sentence_1 sentence_2
    print result
