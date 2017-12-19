module HParser.Generator (printParser, saveParser, genParser, checkSets, generatingSets) where

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

saveParser :: String -> Grammar -> IO()
saveParser filename grammar = writeFile filename $ genParser grammar

genParser :: Grammar -> String
genParser grammar 
   | syntaxSanity grammar /= ""                   -- Syntax check
      = errorWithoutStackTrace (syntaxErrorText++"\n"++(syntaxSanity grammar))
   | checkSets grammar /= ""                      -- Conflict check
      = trace ("{-\n"++confictWarningText++"\n"++(checkSets grammar)++"\n-}") parser
   | otherwise = parser
   where
      syntaxErrorText     = "Syntactic error(s) in grammar definition."
      confictWarningText  = "*** Warning: Conflict(s) in grammar definition.\n" ++
                            "    Notice that this will cause the parser to throw a warning \n" ++
                            "    when running it and make it choose the first rule."
      parser = "\nimport HParser.Parser\n\n" ++
               "data NonTerminal = " ++ (intercalate " | " (nonTerminalNames grammar)) ++ "\n\n" ++
               "instance Show NonTerminal where\n" ++
               showRules ++ 
               "instance Symbol NonTerminal where" ++
               eofRules ++
               "\n   parseEOF _ = False\n" ++ 
               parseRules ++
               "\n   parseRule _ _ = parseFailure\n\n" ++
               "parser :: [Char] -> (Bool, [Char], [Char])\n" ++
               "parser t = (status && (tokens == []), accepted, tokens)\n" ++
               "   where\n" ++
               "      (status, tokens, accepted, tree) = parse S (True, t, [], Node \"\" [])\n\n" ++
               "parseTree :: [Char] -> ParseTree\n" ++
               "parseTree t\n" ++
               "   | status && (tokens == []) = tree\n" ++
               "   | otherwise = trace \"*** Warning! The string was not accepted by the parser.\\n\" tree\n" ++
               "   where\n" ++
               "      (status, tokens, accepted, tree) = parse S (True, t, [], Node \"\" [])\n\n"
      (parseRules, eofRules) = parserRules grammar
      showRules = (concat ["   " ++ "show " ++ name ++ " = \"" ++ name ++ "\"\n" | name <- (nonTerminalNames grammar)]) ++ "\n"

parserRules :: Grammar -> (String, String)
parserRules (Grammar rules) 
   = (concat $ map (parseRulesOfRule (Grammar rules)) rules, concat $ map (parseEOFsOfRule (Grammar rules)) rules)
   where
      parseRulesOfRule grammar rule
         = concat $ S.toList $ S.map (parseRuleOfToken rule) (generatingSet grammar rule)
      parseEOFsOfRule grammar rule
         = concat $ S.toList $ S.map (parseEOFsOfToken rule) (generatingSet grammar rule)
      
      parseRuleOfToken _ "" = "" 
      parseRuleOfToken (Rule (NonTerminal symbol) rh) token 
         = "\n   parseRule " ++ symbol ++ " '" ++ token ++ "' = "++(parserActionsOfRule rh)
      parseRuleOfToken _ _ = ""
      parseEOFsOfToken (Rule (NonTerminal symbol) rh) ""
         = "\n   parseEOF " ++ symbol ++ " = True"
      parseEOFsOfToken _ _ = ""
      
      parserActionsOfRule [] = "parseEpsilon"
      parserActionsOfRule symbols = (intercalate " >>> " $ map symbolAction $ symbols)
      symbolAction (NonTerminal nt) = "parse " ++ nt
      symbolAction (Terminal t) = "parseToken '" ++ t ++ "'"


checkSets :: Grammar -> String
checkSets grammar 
   = seperateLines 
      [conflict (Rule lhs1 rhs1) (Rule lhs2 rhs2) | 
      (set1, (Rule lhs1 rhs1)) <- sets, 
      (set2, (Rule lhs2 rhs2)) <- sets, 
      lhs1 == lhs2, 
      (Rule lhs1 rhs1) < (Rule lhs2 rhs2), 
      (S.intersection set1 set2) /= S.empty]
   where
      sets = generatingSets grammar
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
