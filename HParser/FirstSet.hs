{-|
Set of functions to compute first sets in a 'HParser.Grammar.Grammar'.
-}
module HParser.FirstSet (
   ruleFirstSet, symbolFirstSet, firstSets
   ) where

import HParser.Grammar
import Debug.Trace
import qualified Data.Set as S


{-| 
Computes the first set of a rule in a grammar.
If the resulting set contains the empty string (""), epsilon is in the set.

Returns a set of strings, the names of the terminals in the first set.

>>> grammar
S 	-> E Sp
Sp 	-> ε
Sp 	-> 'PLUS' S
E 	-> 'ONE'
E 	-> 'OPEN' S 'CLOSE'
>>> rule
S 	-> E Sp
>>> ruleFirstSet grammar rule
fromList ["ONE","OPEN"]
-}
ruleFirstSet :: Grammar ->  Rule -> S.Set String          -- Computes the first set of a rule
-- Left-recursion checking
ruleFirstSet grammar rule
   | leftRecursive grammar rule = errorWithoutStackTrace $ "The following rule is left recursive: \n   "++(show rule)
      where
         leftRecursive grammar (Rule lh []) = False
         leftRecursive grammar (Rule lh (symbol:symbols))
            | S.member symbol (nonTerminalFirstSet grammar symbol) = True
            | S.member "" (symbolFirstSet grammar symbol) = leftRecursive grammar (Rule lh symbols)
            | otherwise = False
-- The first set of A -> BC is first{B}, union first{C} if epsilon in first{B}
-- We filter out epsilon until the last symbol
ruleFirstSet grammar (Rule lh [symbol]) = symbolFirstSet grammar symbol
ruleFirstSet grammar (Rule lh (symbol:symbols))
   | S.member "" firstB = S.union (S.filter (/= "") $ firstB) firstC
   | otherwise          = firstB
      where 
        firstB = symbolFirstSet grammar symbol
        firstC = ruleFirstSet grammar (Rule lh symbols)
-- The first-set of A -> epsilon is {epsilon}
ruleFirstSet grammar (Rule lh []) 
   = S.singleton ""


{-| 
Computes the first set of a symbol in a grammar. 
If the resulting set contains the empty string (""), epsilon is in the set.
This includes terminals, which have a trivial solution (singleton set of itself).

Returns a set of strings, the names of the terminals in the first set.

>>> grammar
S 	-> E Sp
Sp 	-> ε
Sp 	-> 'PLUS' S
E 	-> 'ONE'
E 	-> 'OPEN' S 'CLOSE'
>>> symbolFirstSet grammar (NonTerminal Sp)
fromList ["","PLUS"]
>>> symbolFirstSet grammar (Terminal "ONE")
fromList ["ONE"]
-}
symbolFirstSet :: Grammar -> Symbol -> S.Set String       -- Computes the first set of a symbol
-- The first-set of A is the union of the first sets of all rules A -> ..
symbolFirstSet (Grammar s rules) (NonTerminal symbol) 
   = S.unions $ map (ruleFirstSet (Grammar s rules)) rulesA
      where
         rulesA = [Rule (NonTerminal nt) rhSide | Rule (NonTerminal nt) rhSide <- rules, nt == symbol ] -- All rules A -> ..
-- The first-set of a terminal is itself as a singleton
symbolFirstSet (Grammar s rules) (Terminal symbol)
   = S.singleton symbol
-- Same for epsilon
symbolFirstSet (Grammar s rules) Epsilon
   = S.singleton ""


{-| 
/For testing and debugging purposes./
Combination function for the first sets of all rules in the grammar.
If the resulting set contains the empty string (""), epsilon is in the set.

Returns a list of sets of strings, one set per rule in the grammar, in the same order.

>>> grammar
S 	-> E Sp
Sp 	-> ε
Sp 	-> 'PLUS' S
E 	-> 'ONE'
E 	-> 'OPEN' S 'CLOSE'
>>> firstSets grammar
[fromList ["ONE","OPEN"],fromList [""],fromList ["PLUS"],fromList ["ONE"],fromList ["OPEN"]]
-}
firstSets :: Grammar -> [S.Set String]
firstSets (Grammar s rules) = map (ruleFirstSet (Grammar s rules)) rules



-- HELPER FUNCTIONS
nonTerminalFirstSet :: Grammar -> Symbol -> S.Set Symbol  -- Finds all B in A -*> BC
nonTerminalFirstSet grammar (NonTerminal nt) 
   = recursionStep grammar $ nonTerminalFirstSetStep grammar (NonTerminal nt)
   where
      nonTerminalFirstSetStep (Grammar s rules) symbol 
         = S.fromList [NonTerminal x | Rule (NonTerminal nt) ((NonTerminal x):rhs) <- rules, (NonTerminal nt) == symbol]
      recursionStep grammar set
         | recursionSet `S.isSubsetOf` set   = set
         | otherwise                         = recursionStep grammar (S.union recursionSet set)
            where
               recursionSet = (S.foldr S.union S.empty) $ S.map (nonTerminalFirstSetStep grammar) set
nonTerminalFirstSet grammar _ = S.empty
