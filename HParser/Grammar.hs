module HParser.Grammar (
   Symbol (..), Rule (..), Grammar (..), 
   nonTerminalNames, terminalNames, syntaxSanity
   ) where

import Data.List
import qualified Data.Set as Set

data Symbol = Terminal String | NonTerminal String | Epsilon
data Rule = Rule Symbol [Symbol]
data Grammar = Grammar Symbol [Rule]

instance Show Symbol where
   show (NonTerminal c) = c
   show (Terminal c) = ("'"++c++"'")
   show Epsilon = "\949"

instance Show Rule where
   show (Rule start []) = (show start) ++ " \t-> " ++ (show Epsilon)
   show (Rule start tokens) = (show start) ++ " \t-> " ++ (intercalate " " $ map show $ tokens)

instance Show Grammar where
   show (Grammar s rules) = (intercalate "\n" $ map show $ rules)

instance Eq Symbol where
   (==) (NonTerminal a) (NonTerminal b) = a == b
   (==) (Terminal a) (Terminal b) = a == b
   (==) Epsilon Epsilon = True
   (==) _ _ = False

instance Eq Rule where
   (==) (Rule lhs1 rhs1) (Rule lhs2 rhs2) = lhs1 == lhs2 && rhs1 == rhs2

instance Ord Symbol where
   (NonTerminal nt1) `compare` (NonTerminal nt2) = nt1 `compare` nt2
   (Terminal t1) `compare` (Terminal t2) = t1 `compare` t2
   Epsilon `compare` _ = LT
   _ `compare` Epsilon = GT
   (Terminal _) `compare` (NonTerminal _) = LT
   (NonTerminal _) `compare` (Terminal _) = GT

instance Ord Rule where
   (Rule lhs1 rhs1) `compare` (Rule lhs2 rhs2)
      | lhs1 == lhs2 = rhs1 `compare` rhs2
      | otherwise = lhs1 `compare` lhs2

nonTerminalNames :: Grammar -> [String]
nonTerminalNames (Grammar s rules) = nub [name | Rule (NonTerminal name) rh <- rules]

terminalNames :: Grammar -> [String]
terminalNames (Grammar s rules) = [name | Terminal name <- concat [rh | Rule s rh <- rules]]

startSymbol :: Grammar -> String
startSymbol (Grammar (NonTerminal start) rules) = start
startSymbol _ = ""

syntaxSanity :: Grammar -> String
syntaxSanity (Grammar s rules) = seperateLines ((checkGrammarSanity (Grammar s rules)):(map checkRuleSanity rules))
   where
      checkGrammarSanity :: Grammar -> String
      checkGrammarSanity grammar
                       | startSymbol grammar == "" = "Error: Start symbol must be a non-terminal"
                       | otherwise = ""

      checkRuleSanity :: Rule -> String
      checkRuleSanity (Rule (NonTerminal nt) rhs) = seperateLines $ map checkSymbolSanity rhs
      checkRuleSanity (Rule lhs rhs)              = seperateLines $ (syntaxError (Rule lhs rhs)):(map checkSymbolSanity rhs)
      
      checkSymbolSanity :: Symbol -> String
      checkSymbolSanity t = ""
      {-
      checkSymbolSanity (Terminal t)
         | length t == 1  = ""
         | otherwise      = "A terminal with value '"++t++"' is not valid at this moment."
      checkSymbolSanity _ = ""
      -}
      
      syntaxError rule = "The following rule has an invalid left hand side: \n   "++(show rule)
      seperateLines (x:[]) = x
      seperateLines (x:xs)
         | x == ""    = seperateLines xs
         | otherwise  = x++"\n"++(seperateLines xs)
      seperateLines [] = ""

