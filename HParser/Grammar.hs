{-|
This module defined the core data types used, specifically grammars.
-}
module HParser.Grammar (
   Symbol (..), Rule (..), Grammar (..), 
   nonTerminalNames, terminalNames, startSymbol, syntaxSanity
   ) where

import Data.List
import qualified Data.Set as Set


{-| 
Represents a symbol in a grammar, in other word either a terminal, non-terminal or epsilon.

Symbol is an instance is Show:

>>> Terminal "TOKEN"
'TOKEN'
>>> NonTerminal S
S
>>> Epsilon
ε
-}
data Symbol = Terminal String | NonTerminal String | Epsilon
instance Show Symbol where
   show (NonTerminal c) = c
   show (Terminal c) = ("'"++c++"'")
   show Epsilon = "\949"
instance Eq Symbol where
   (==) (NonTerminal a) (NonTerminal b) = a == b
   (==) (Terminal a) (Terminal b) = a == b
   (==) Epsilon Epsilon = True
   (==) _ _ = False
instance Ord Symbol where
   (NonTerminal nt1) `compare` (NonTerminal nt2) = nt1 `compare` nt2
   (Terminal t1) `compare` (Terminal t2) = t1 `compare` t2
   Epsilon `compare` _ = LT
   _ `compare` Epsilon = GT
   (Terminal _) `compare` (NonTerminal _) = LT
   (NonTerminal _) `compare` (Terminal _) = GT


{-|
Respresents a single rule in a grammar. 
Note that a non-terminal can be in the left-hand side of multiple rules in a grammar.

Rule is an instance is Show:

>>> Rule (NonTerminal "E") [(Terminal "OPEN"), (NonTerminal "S"), (Terminal "CLOSE")]
E 	-> 'OPEN' S 'CLOSE'
-}
data Rule = Rule Symbol [Symbol]
instance Show Rule where
   show (Rule start []) = (show start) ++ " \t-> " ++ (show Epsilon)
   show (Rule start tokens) = (show start) ++ " \t-> " ++ (intercalate " " $ map show $ tokens)
instance Eq Rule where
   (==) (Rule lhs1 rhs1) (Rule lhs2 rhs2) = lhs1 == lhs2 && rhs1 == rhs2
instance Ord Rule where
   (Rule lhs1 rhs1) `compare` (Rule lhs2 rhs2)
      | lhs1 == lhs2 = rhs1 `compare` rhs2
      | otherwise = lhs1 `compare` lhs2


{-|
Represents a single cohesive set of rules, in other words a grammar. For example:

@
grammar = Grammar (NonTerminal \"S\") [
   Rule (NonTerminal \"S\") [(NonTerminal \"E\"), (NonTerminal \"Sp\")],
   Rule (NonTerminal \"Sp\") [],
   Rule (NonTerminal \"Sp\") [(Terminal \"PLUS\"), (NonTerminal \"S\")],
   Rule (NonTerminal \"E\") [(Terminal \"ONE\")],
   Rule (NonTerminal \"E\") [(Terminal \"OPEN\"), (NonTerminal \"S\"), (Terminal \"CLOSE\")]
   ]
@

Grammar is an instance is Show:

>>> grammar
S 	-> E Sp
Sp 	-> ε
Sp 	-> 'PLUS' S
E 	-> 'ONE'
E 	-> 'OPEN' S 'CLOSE'
-}
data Grammar = Grammar Symbol [Rule]
instance Show Grammar where
   show (Grammar s rules) = (intercalate "\n" $ map show $ rules)


{-|
Returns all non-terminals in a grammar, as a list of strings (symbol names).
-}
nonTerminalNames :: Grammar -> [String]
nonTerminalNames (Grammar s rules) = nub [name | Rule (NonTerminal name) rh <- rules]


{-|
Returns all terminals in a grammar, as a list of strings (symbol names).
-}
terminalNames :: Grammar -> [String]
terminalNames (Grammar s rules) = [name | Terminal name <- concat [rh | Rule s rh <- rules]]

{-|
Returns the starting symbol of a grammar. Returns empty stirng if one is not properly defined.
-}
startSymbol :: Grammar -> String
startSymbol (Grammar (NonTerminal start) rules) = start
startSymbol _ = ""

{-|
Checks whether a grammar has been properly defined. It does the following checks:
   
   1. Check whether every rule has a non-terminal left-hand side.
   2. Check whether the starting symbol is a non-terminal.

For example:

@
grammar = Grammar (Terminal \"Token\") [
   Rule (NonTerminal \"S\") [(Terminal \"Token\")],
   Rule (Terminal \"Token\") []
   ]
@

>>> grammar
S 	-> 'Token'
'Token' 	-> ε
>>> putStr $ syntaxSanity grammar
Error: Start symbol must be a non-terminal
The following rule has an invalid left hand side: 
   'Token' 	-> ε
-}
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

