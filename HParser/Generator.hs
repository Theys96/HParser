module HParser.Generator (printParser, genParser, generatingSets) where

import qualified Data.Set as S
import HParser.FirstSet
import HParser.FollowSet
import HParser.Grammar
import Data.List
import Debug.Trace
{-

-}
printParser :: Grammar -> IO()
printParser grammar = putStr $ genParser grammar

genParser :: Grammar -> String
genParser grammar 
   | syntaxSanity grammar /= ""                   -- Syntax check
      = errorWithoutStackTrace (syntaxErrorText++"\n"++(syntaxSanity grammar))
   | checkSets (generatingSets grammar) /= ""     -- Conflict check
      = trace ("{-\n"++confictWarningText++"\n"++(checkSets $ generatingSets $ grammar)++"\n-}") parser
   | otherwise = parser
   where
      syntaxErrorText     = "Syntactic error(s) in grammar definition."
      confictWarningText  = "*** Warning: Conflict(s) in grammar definition.\n" ++
                            "    Notice that this will cause the parser to throw a warning \n" ++
                            "    when running it and make it choose the first rule."
      parser = "\n" ++
               "data NonTerminal = " ++ (intercalate " | " (nonTerminalNames grammar)) ++ "\n\n" ++
               "parseToken :: Char -> (Bool, [Char], [Char]) -> (Bool, [Char], [Char])\n" ++
               "parseToken x (True, y:t, a)\n" ++ 
               "                          | x == y    = (True, t, a++[x])\n" ++
               "                          | otherwise = (False, y:t, a)\n" ++
               "parseToken _ (False, t, a)   = (False, t, a)\n" ++
               "parseToken _ (True, [], a)   = (False, [], a)\n\n" ++
               "parse :: NonTerminal -> (Bool, [Char], [Char]) -> (Bool, [Char], [Char])\n" ++
               "parse _ (False, ts, a) = (False, ts, a)\n" ++
            (parserRules grammar) ++
               "parse _ (_, ts, a) = (False, ts, a)\n\n" ++
               "parser :: [Char] -> (Bool, [Char], [Char])\n" ++
               "parser t = (status && (tokens == []), accepted, tokens)\n" ++
               "   where\n" ++
               "      (status, tokens, accepted) = parse S (True, t, [])\n\n" ++
               ""

parserRules :: Grammar -> String
parserRules (Grammar rules) = concat $ map (parserRulesOfRule (Grammar rules)) rules
   where
      parserRulesOfRule grammar rule
         = concat $ S.toList $ S.map (parserRuleOfToken rule) (generatingSet grammar rule)
      parserRuleOfToken (Rule (NonTerminal symbol) rh) ""
         = "parse " ++ symbol ++ " (True, [], accepted) = "++(parserActionsOfRule rh)++" (True, [], accepted)\n"
      parserRuleOfToken (Rule (NonTerminal symbol) rh) token 
         = "parse " ++ symbol ++ " (True, '" ++ token ++ "':tokens, accepted) = "++(parserActionsOfRule rh)++"(True, '" ++ token ++ "':tokens, accepted)\n"
      parserActionsOfRule [] = ""
      parserActionsOfRule symbols = (intercalate " $ " $ map symbolAction $ reverse symbols)++" $ "
      symbolAction (NonTerminal nt) = "parse " ++ nt
      symbolAction (Terminal t) = "parseToken '" ++ t ++ "'"


checkSets :: [(S.Set String, Rule)] -> String
checkSets sets 
   = seperateLines 
      [conflict (Rule lhs1 rhs1) (Rule lhs2 rhs2) | 
      (set1, (Rule lhs1 rhs1)) <- sets, 
      (set2, (Rule lhs2 rhs2)) <- sets, 
      lhs1 == lhs2, 
      (Rule lhs1 rhs1) < (Rule lhs2 rhs2), 
      (S.intersection set1 set2) /= S.empty]
   where
      conflict rule1 rule2 = "In the rules: \n   "++(show rule1)++"\n   "++(show rule2)
      seperateLines (x:[]) = x
      seperateLines (x:xs)
         | x == ""    = seperateLines xs
         | otherwise  = x++"\n"++(seperateLines xs)
      seperateLines [] = ""

generatingSets :: Grammar -> [(S.Set String, Rule)]
generatingSets (Grammar rules) = [(generatingSet (Grammar rules) (Rule lhs rhs), (Rule lhs rhs)) | (Rule lhs rhs) <- rules]

generatingSet :: Grammar -> Rule -> S.Set String
generatingSet grammar (Rule lh rh)
   | S.member "" (ruleFirstSet grammar rule) = 
      S.union thisFirstSet (followSet grammar lh)
   | otherwise = thisFirstSet
      where
         thisFirstSet = (S.filter (/= "") $ ruleFirstSet grammar rule) 
         rule = (Rule lh rh)
