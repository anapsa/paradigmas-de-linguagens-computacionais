--Altura da Árvore
data Tree t = Node t (Tree t) (Tree t) 
              | Nilt
              deriving (Read)
              
alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0 
alturaArvore (Node node right left) = 1 + max (alturaArvore(right)) (alturaArvore(left))
main = do
       a <- getLine
       let result = alturaArvore (read a::Tree Int)
       print result
