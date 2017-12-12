module HParser.Grammar (
   Symbol (..), Rule (..), Grammar (..), 
   nonTerminalNames, syntaxSanity
   ) where

import Data.List
import qualified Data.Set as Set

data Symbol = Terminal String | NonTerminal String | Epsilon
data Rule = Rule Symbol [Symbol]
data Grammar = Grammar [Rule]

instance Show Symbol where
   show (NonTerminal c) = c
   show (Terminal c) = ("'"++c++"'")
   show Epsilon = "\949"

instance Show Rule where
   show (Rule start []) = (show start) ++ " \t-> " ++ (show Epsilon)
   show (Rule start tokens) = (show start) ++ " \t-> " ++ (intercalate " " $ map show $ tokens)

instance Show Grammar where
   show (Grammar rules) = (intercalate "\n" $ map show $ rules)

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
nonTerminalNames (Grammar rules) = nub [name | Rule (NonTerminal name) rh <- rules]

syntaxSanity :: Grammar -> String
syntaxSanity (Grammar rules) = seperateLines (map checkRuleSanity rules)
   where
      checkRuleSanity :: Rule -> String
      checkRuleSanity (Rule (NonTerminal nt) rhs) = seperateLines $ map checkSymbolSanity rhs
      checkRuleSanity (Rule lhs rhs)              = seperateLines $ (syntaxError (Rule lhs rhs)):(map checkSymbolSanity rhs)
      
      checkSymbolSanity :: Symbol -> String
      checkSymbolSanity (Terminal t)
         | length t == 1  = ""
         | otherwise      = "A terminal with value '"++t++"' is not valid at this moment."
      checkSymbolSanity _ = ""
      
      syntaxError rule = "The following rule has an invalid left hand side: \n   "++(show rule)
      seperateLines (x:[]) = x
      seperateLines (x:xs)
         | x == ""    = seperateLines xs
         | otherwise  = x++"\n"++(seperateLines xs)
      seperateLines [] = ""

