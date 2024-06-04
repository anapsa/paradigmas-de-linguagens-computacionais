--DNA2
data Animal = Cisnal | Iguanoide | Narvale | Null
  deriving (Eq, Show)
  
produtoEscalar :: String -> String -> Int -> Int -> Double 
produtoEscalar [] _ qtd 0 = 0
produtoEscalar _ [] qtd 0 = 0
produtoEscalar [] [] qtd tamanho = fromIntegral qtd / fromIntegral tamanho
produtoEscalar [] (y:ys) qtd tamanho = produtoEscalar [] ys qtd (tamanho+1)
produtoEscalar (x:xs) [] qtd tamanho = produtoEscalar xs [] qtd (tamanho+1)
produtoEscalar (x:xs) (y:ys) qtd tamanho
    | x == y = produtoEscalar xs ys (qtd+1) (tamanho+1) 
    | otherwise = produtoEscalar xs ys qtd (tamanho+1)

count :: [Double] -> Int -> Int -> Int -> [Int] 
count [] narvale cisnal iguanoide = [cisnal] ++ [iguanoide] ++ [narvale] 
count (x:xs) narvale cisnal iguanoide
    | x >= 0.1 && x <= 0.3 = count xs narvale (cisnal+1) iguanoide
    | x >= 0.4 && x <= 0.7 = count xs narvale cisnal (iguanoide+1)
    | x >= 0.8 = count xs (narvale+1) cisnal iguanoide
    | otherwise = count xs narvale cisnal iguanoide


dna2 :: [String] -> [String] -> [Int]
dna2 xs ys = count (criaLista xs ys) 0 0 0
    where 
        criaLista [] _ = [] 
        criaLista _ [] = [] 
        criaLista (x:xs) (y:ys) = (produtoEscalar x y 0 0) : criaLista xs ys 

main = do
  firstExtract <- words <$> getLine                       -- equivalente a (read firstExtract :: [String])
  secondExtract <- words <$> getLine
  let result = dna2 firstExtract secondExtract
  print result
