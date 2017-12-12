module HParser.FirstSet (
   firstSets, ruleFirstSet, symbolFirstSet
   ) where

import HParser.Grammar
import Debug.Trace
import qualified Data.Set as S

firstSets :: Grammar -> [S.Set String]                 -- Combination function for the first sets of all rules in the grammar
ruleFirstSet :: Grammar ->  Rule -> S.Set String       -- Computes the first set of a rule
symbolFirstSet :: Grammar -> Symbol -> S.Set String    -- Computes the first set of a symbol

firstSets (Grammar rules) = map (ruleFirstSet (Grammar rules)) rules

-- The first-set of A is the union of the first sets of all rules A -> ..
symbolFirstSet (Grammar rules) (NonTerminal symbol) 
   = S.unions $ map (ruleFirstSet (Grammar rules)) rulesA
      where
         rulesA = [Rule (NonTerminal nt) rhSide | Rule (NonTerminal nt) rhSide <- rules, nt == symbol ] -- All rules A -> ..

-- The first-set of a terminal is itself as a singleton
symbolFirstSet (Grammar rules) (Terminal symbol)
   = S.singleton symbol
-- Same for epsilon
symbolFirstSet (Grammar rules) Epsilon
   = S.singleton ""

-- Find all B in A -*> BC, used by left-recusion checking
nonTerminalFirstSet grammar (NonTerminal nt) 
   = recursionStep grammar $ nonTerminalFirstSetStep grammar (NonTerminal nt)
   where
      nonTerminalFirstSetStep (Grammar rules) symbol 
         = S.fromList [NonTerminal x | Rule (NonTerminal nt) ((NonTerminal x):rhs) <- rules, (NonTerminal nt) == symbol]
      recursionStep grammar set
         | recursionSet `S.isSubsetOf` set   = set
         | otherwise                         = recursionStep grammar (S.union recursionSet set)
            where
               recursionSet = (S.foldr S.union S.empty) $ S.map (nonTerminalFirstSetStep grammar) set
nonTerminalFirstSet grammar _ = S.empty

-- Left-recursion checking
ruleFirstSet grammar rule
   | leftRecursive grammar rule = errorWithoutStackTrace $ "The following rule is left recursive: \n   "++(show rule)
      where
         leftRecursive grammar (Rule lh []) = False
         leftRecursive grammar (Rule lh (symbol:symbols)) = S.member symbol (nonTerminalFirstSet grammar symbol)

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
