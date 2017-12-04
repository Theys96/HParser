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
         rulesA = [Rule nt rhSide | Rule nt rhSide <- rules, nt == (NonTerminal symbol) ] -- All rules A -> ..

-- The first-set of a terminal is itself as a singleton
symbolFirstSet (Grammar rules) (Terminal symbol)
   = S.singleton symbol 

-- The first set of A -> BC is first{B}, union first{C} if epsilon in first{B}
ruleFirstSet grammar (Rule lh ((NonTerminal name):symbols))
   | S.member "" firstB = S.union firstB firstC
   | otherwise          = firstB
      where 
        firstB = symbolFirstSet grammar (NonTerminal name)
        firstC = ruleFirstSet grammar (Rule lh symbols)

-- The first-set of A -> aB for terminal a is {a}
ruleFirstSet grammar (Rule lh ((Terminal name):symbols))
   = S.singleton name

-- The first-set of A -> epsilon is {epsilon}
ruleFirstSet grammar (Rule lh []) 
   = S.singleton ""
