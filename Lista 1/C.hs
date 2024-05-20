--HexaToBin
dtob :: Char -> String
dtob '0' = "0000"
dtob  '1' = "0001"
dtob  '2' = "0010"
dtob  '3' = "0011"
dtob  '4' = "0100"
dtob  '5' = "0101"
dtob  '6' = "0110"
dtob  '7' = "0111"
dtob  '8' = "1000"
dtob  '9' = "1001"
dtob  'A' = "1010"
dtob  'B' = "1011"
dtob  'C' = "1100"
dtob  'D' = "1101"
dtob  'E' = "1110"
dtob  'F' = "1111"

htob :: String -> String
htob [] = [] 
htob (x:xs) = dtob x ++ htob xs
 
main = do
    s <- getLine
    let result = htob s
    print result
