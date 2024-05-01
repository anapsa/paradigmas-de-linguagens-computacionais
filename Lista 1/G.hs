addZero::[Int] -> [Int]
addZero [] = []
addZero (x:xs) = 0 : addZero xs


mul2 :: [Int] -> [Int] -> [Int]
mul2 [] [] = []
mul2 xs []  = addZero xs
mul2 [] ys = addZero ys 
mul2 (x:xs) (y:ys) = x * y : mul2 xs ys

main = do
    sa <- getLine
    let a = read sa :: [Int]
    sb <- getLine
    let b = read sb :: [Int]
    let result = mul2 a b
    print result
