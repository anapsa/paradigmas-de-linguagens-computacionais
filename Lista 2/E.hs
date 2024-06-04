--Robô 2
data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)
direita :: Direction -> Direction
direita North = East 
direita South = West
direita West = North 
direita East = South 

esquerda :: Direction -> Direction 
esquerda North = West 
esquerda South = East 
esquerda West = South 
esquerda East = North 
                 
faces :: Direction -> [Command] -> Direction
faces dir [] = dir
faces dir (x:xs)
            | x == Forward 0 = faces dir xs 
            | x == Backward 0 = faces dir xs 
            | x == TurnLeft = faces (esquerda dir) xs
            | x == TurnRight = faces (direita dir) xs
            | otherwise = faces dir xs 

main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result
