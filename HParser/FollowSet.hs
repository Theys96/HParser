module HParser.FollowSet (
   followSet
   ) where

import HParser.Grammar
import HParser.FirstSet
import qualified Data.Set as S

-- Follow Sets:
-- $ in Follow{S}
-- if A -> aBC and C = ε or ε in First{C} then Follow{A} in Follow{C}   | followSetSubsets
-- if A -> aBC then First{C}\{ε} in Follow{B}                           | directFollowSets

-- Computes the follow set of a given non-terminal in a grammar
followSet :: Grammar -> Symbol -> S.Set (String)
followSet grammar nonTerminal = 
   S.union
      (directFollowSet grammar nonTerminal)
      (S.unions $ S.toList $ S.map (directFollowSet grammar)
         $ recursiveFollowSetSubsets grammar nonTerminal)


-- Recursive version of 'followSetSubsets'; It repeats that function as long as the resulting set is larger
-- 'recursiveFollowSetSubsets2' is the recursive method that works on sets. 
-- 'recursiveFollowSetSubsets' is the wrapper function for a single nonterminal that is applied as a singleton.
recursiveFollowSetSubsets :: Grammar -> Symbol -> S.Set (Symbol)
recursiveFollowSetSubsets grammar nonTerminal = recursiveFollowSetSubsets2 grammar (S.singleton nonTerminal)

recursiveFollowSetSubsets2 :: Grammar -> S.Set (Symbol) -> S.Set (Symbol)
recursiveFollowSetSubsets2 grammar set
   | recursionSet `S.isSubsetOf` set        = set
   | otherwise                              = recursiveFollowSetSubsets2 grammar (S.union recursionSet set)
      where
         recursionSet = (S.foldr S.union S.empty) $ S.map (followSetSubsets grammar) set


-- Set of all non-terminals that may produce the target non-terminal as the last symbol on the right-hand side of a rule
-- This function only finds direct rules, the recursive wrapper must be used for infinite recursion
followSetSubsets :: Grammar -> Symbol -> S.Set (Symbol)
followSetSubsets (Grammar s rules) nonTerminal = S.unions (map (ruleFollowSubsets nonTerminal) rules)
   where
      -- Cases for A -> aBC (Rule), looking for non-terminal D (Symbol)
      ruleFollowSubsets :: Symbol -> Rule -> S.Set (Symbol)
      ruleFollowSubsets nonTerminal (Rule a rh)
         | rh == []                = S.empty                        -- aBC = ε
         | c == nonTerminal        = S.singleton a                  -- C = D <==> B = D and C = ε
         | S.member "" (symbolFirstSet (Grammar s rules) c)           -- ε in First{C}
                                   = ruleFollowSubsets nonTerminal (Rule a (init rh))
         | otherwise               = S.empty
            where
               c = last rh


-- Set of all terminals that can be directly found as terminals or first-sets of non-terminals after the target in any rule
-- directFollowSet2 is the "pure" version of this function, whereas directFollowSet adds "" (EOF) to the set for NonTerminal S
directFollowSet :: Grammar -> Symbol -> S.Set (String)
directFollowSet2 :: Grammar -> Symbol -> S.Set (String)

directFollowSet grammar (NonTerminal "S") = S.union (S.singleton "") (directFollowSet2 grammar (NonTerminal "S"))
directFollowSet grammar (NonTerminal nt) = directFollowSet2 grammar (NonTerminal nt)

directFollowSet2 (Grammar s rules) nonTerminal = S.filter (/= "") $ S.unions (map checkRule rules) 
   where
      checkRule :: Rule -> S.Set (String)
      checkRule (Rule lh (nt:x:xs))
         | nt == nonTerminal  = S.union checkNextRule firstSetC -- nt is our target, add First{x:xs}
         | otherwise          = checkNextRule                   -- to the next symbol
            where
               checkNextRule = checkRule c
               firstSetC = ruleFirstSet (Grammar s rules) c
               c = Rule lh (x:xs)
      checkRule (Rule lh rh) = S.empty
