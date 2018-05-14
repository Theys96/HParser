import Parser
import Data.Tree

main = do  
    putStrLn "Enter an expression:"  
    exp <- getLine  
    putStr $ calcTree $ parseTree (toTokenList exp)

toTokenList :: String -> [TokenTuple]
toTokenList [] = []
toTokenList (c:list) = (charToken c, [c]):(toTokenList list)
   where
      charToken :: Char -> Token
      charToken '(' = OPEN
      charToken ')' = CLOSE
      charToken '+' = PLUS
      charToken '0' = NUM
      charToken '1' = NUM
      charToken '2' = NUM
      charToken '3' = NUM
      charToken '4' = NUM
      charToken '5' = NUM
      charToken '6' = NUM
      charToken '7' = NUM
      charToken '8' = NUM
      charToken '9' = NUM


calcTree :: ParseTree -> Integer
calcTree (Node (T NUM string) c) = read string :: Int
calcTree (Node (NT S) [a, b]) = (calcTree a) + (calcTree b)
calcTree (Node (NT Sp) [plus, s]) = calcTree s
calcTree (Node (NT E) [open, s, open]) = calcTree s
calcTree (Node (NT E) [num]) = calcTree num
