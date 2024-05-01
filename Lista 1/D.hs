bSort :: [String] -> [String]
bSort [] = []
bSort (x:xs) = bSort[a | a <- xs, a < x]  ++ [x] ++ bSort[a | a <- xs, a >= x] 

main = do
       a <- getLine
       let result = bSort (read a :: [String])
       print result
