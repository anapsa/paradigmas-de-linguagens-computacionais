--Fatores Primos
fatPrime :: Int -> [(Int, Int)]
fatPrime 1 = []
fatPrime num = fatorar num (2, 0)
  where
    fatorar num (divi, qtd)
      | num < divi && qtd > 0 = [(divi, qtd)] 
      | num < divi = []
      | num `mod` divi == 0 = fatorar (num `div` divi) (divi, qtd + 1)
      | qtd > 0 = (divi, qtd) : fatorar num (divi + 1, 0)
      | otherwise = fatorar num (divi + 1, 0)

main :: IO ()
main = do
  a <- getLine
  let result = fatPrime (read a :: Int)
  print result
