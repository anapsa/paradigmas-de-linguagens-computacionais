--DNA1 
data Tree t = Node t (Tree t) (Tree t) | Nilt
  deriving (Read, Show)
  
converte :: Int -> Char 
converte 0 = 'E' 
converte 1 = 'M' 
converte 2 = 'A' 
converte 3 = 'C'
converte 4 = 'S' 

dna1 :: Tree Int -> [String]
dna1 t = separa (criaString t) 
    where 
        criaString Nilt = []
        criaString (Node n left right) = criaString left ++ [converte (n `mod` 5)] ++ criaString right
        
        separa [] = []
        separa xs = take 8 xs : separa (drop 8 xs)
      
        
main :: IO ()
main = do

  input <- getLine

  let result = dna1 (read input :: Tree Int)

  print result
