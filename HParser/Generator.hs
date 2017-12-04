module HParser.Generator (printParser, genParser) where

import qualified Data.Set as S
import HParser.FirstSet
import HParser.FollowSet
import HParser.Grammar
import Data.List

printParser :: Grammar -> IO()
printParser grammar = putStr $ genParser grammar

genParser :: Grammar -> String
genParser grammar = "\n" ++
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



generatingSets :: Grammar -> [S.Set String]
generatingSets (Grammar rules) = map (generatingSet (Grammar rules)) rules

generatingSet :: Grammar -> Rule -> S.Set String
generatingSet grammar (Rule lh rh)
   | S.member "" (ruleFirstSet grammar rule) = 
      S.union thisFirstSet (followSet grammar lh)
   | otherwise = thisFirstSet
      where
         thisFirstSet = (S.filter (/= "") $ ruleFirstSet grammar rule) 
         rule = (Rule lh rh)
