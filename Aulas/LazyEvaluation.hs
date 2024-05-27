fib :: [Int] 
fib = fibonacci 0 1
    where
        fibonacci a b = a : fibonacci b (a+b)
-- outro jeito 
fib1 :: [Int]
fib1 = map fibonacci1 [0..] 

fibonacci1 :: Int -> Int 
fibonacci1 0 = 1
fibonacci1 1 = 1
fibonacci1 n = fibonacci1 (n-1) + fibonacci1 (n-2)

-- outro jeito 
fib2 :: [Int] 
fib2 = [fibonacci1 x | x <- [0..]]

primes :: [Int] 
primes = remove ([2..]) 
    where 
        remove (x:xs) = x : filter (\y -> y `mod` x /= 0) (remove xs)
            
    
        
main :: IO ()
main = print $ take 20 primes 
