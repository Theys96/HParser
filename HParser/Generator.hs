{-|
Set of functions to generate LL(1) parsers from a 'HParser.Grammar.Grammar'.
-}
module HParser.Generator (genParser, printParser, saveParser, checkSets) where

import qualified Data.Set as S
import HParser.FirstSet
import HParser.FollowSet
import HParser.Grammar
import Data.List
import Debug.Trace

{-|
Uses the 'genParser' function to print a parser to IO.
-}
printParser :: Grammar -> String -> IO()
printParser grammar name = putStr $ genParser grammar name

{-|
Uses the 'genParser' function to save a parser to a file. 
The name that is needed for generating the parser, is also used as the filename.
-}
saveParser :: Grammar -> String -> IO()
saveParser grammar name = writeFile (name++".hs") $ genParser grammar name

{-|
Creates a Haskell LL(1)-parser from given 'HParser.Grammar.Grammar', with a given name.
It will give warnings or errors if the grammar is not properly defined or not LL(1).

Example:

@
>>> grammar
S 	-> E Sp
Sp 	-> ε
Sp 	-> 'PLUS' S
E 	-> 'ONE'
E 	-> 'OPEN' S 'CLOSE'
>>> putStr $ genParser grammar \"Parser\"
module Parser (Token (..), TokenTuple (..), ParseTree (..), parser, parseTree, printParseTree) where

import Data.Tree
import Debug.Trace
import Control.Arrow

-- GRAMMAR-SPECIFIC PARSER CODE
data Token = PLUS | ONE | OPEN | CLOSE
   deriving (Read, Show, Eq)

data NonTerminal = S | Sp | E
   deriving (Read, Show, Eq)

instance Symbol NonTerminal where
   parseEOF Sp = True
   parseEOF _ = False

   parseRule S ONE = parse E >>> parse Sp
   parseRule S OPEN = parse E >>> parse Sp
   parseRule Sp CLOSE = parseEpsilon
   parseRule Sp PLUS = parseToken PLUS >>> parse S
   parseRule E ONE = parseToken ONE
   parseRule E OPEN = parseToken OPEN >>> parse S >>> parseToken CLOSE
   parseRule _ _ = parseFailure

-- Set starting symbol
parser = _parser S
parseTree = _parseTree S


(omitting standard parser code)
@
-}
genParser :: Grammar -> String -> String
genParser grammar name 
   | syntaxSanity grammar /= ""                   -- Syntax check
      = errorWithoutStackTrace (syntaxErrorText++"\n"++(syntaxSanity grammar))
   | checkSets grammar /= ""                      -- Conflict check
      = trace ("{-\n"++confictWarningText++"\n"++(checkSets grammar)++"\n-}") parser
   | otherwise = parser++parserCode
   where
      syntaxErrorText     = "Syntactic error(s) in grammar definition."
      confictWarningText  = "*** Warning: Conflict(s) in grammar definition.\n" ++
                            "    Notice that this will cause the parser to throw a warning \n" ++
                            "    when running it and make it choose the first rule."
      parser = "module "++name++" (Token (..), TokenTuple (..), ParseTree (..), parser, parseTree, printParseTree) where\n\n"++
               "import Data.Tree\nimport Debug.Trace\nimport Control.Arrow\n\n" ++
               "-- GRAMMAR-SPECIFIC PARSER CODE\n" ++
               "data Token = " ++ (intercalate " | " (terminalNames grammar)) ++ "\n" ++
               "   deriving (Read, Show, Eq)\n\n" ++
               "data NonTerminal = " ++ (intercalate " | " (nonTerminalNames grammar)) ++ "\n" ++
               "   deriving (Read, Show, Eq)\n\n" ++
               "instance Symbol NonTerminal where" ++
               eofRules ++
               "\n   parseEOF _ = False\n" ++ 
               parseRules ++
               "\n   parseRule _ _ = parseFailure\n\n" ++
               "-- Set starting symbol\n"++
               "parser = _parser S\n"++
               "parseTree = _parseTree S\n\n\n\n"
      (parseRules, eofRules) = parserRules grammar

parserRules :: Grammar -> (String, String)
parserRules (Grammar s rules) 
   = (concat $ map (parseRulesOfRule (Grammar s rules)) rules, concat $ map (parseEOFsOfRule (Grammar s rules)) rules)
   where
      parseRulesOfRule grammar rule
         = concat $ S.toList $ S.map (parseRuleOfToken rule) (generatingSet grammar rule)
      parseEOFsOfRule grammar rule
         = concat $ S.toList $ S.map (parseEOFsOfToken rule) (generatingSet grammar rule)
      
      parseRuleOfToken _ "" = "" 
      parseRuleOfToken (Rule (NonTerminal symbol) rh) token 
         = "\n   parseRule " ++ symbol ++ " " ++ token ++ " = "++(parserActionsOfRule rh)
      parseRuleOfToken _ _ = ""
      parseEOFsOfToken (Rule (NonTerminal symbol) rh) ""
         = "\n   parseEOF " ++ symbol ++ " = True"
      parseEOFsOfToken _ _ = ""
      
      parserActionsOfRule [] = "parseEpsilon"
      parserActionsOfRule symbols = (intercalate " >>> " $ map symbolAction $ symbols)
      symbolAction (NonTerminal nt) = "parse " ++ nt
      symbolAction (Terminal t) = "parseToken " ++ t


{-|
Function to check for conflicting rules. 

Returns conflicts written in a string.
If a grammar is LL(1), this will return empty string ("").
-}
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
generatingSets (Grammar s rules) = [(generatingSet (Grammar s rules) (Rule lhs rhs), (Rule lhs rhs)) | (Rule lhs rhs) <- rules]

generatingSet :: Grammar -> Rule -> S.Set String
generatingSet grammar (Rule lh rh)
   | S.member "" (ruleFirstSet grammar rule) = 
      S.union thisFirstSet (followSet grammar lh)
   | otherwise = thisFirstSet
      where
         thisFirstSet = (S.filter (/= "") $ ruleFirstSet grammar rule) 
         rule = (Rule lh rh)

parserCode = concat (intersperse "\n" ["",
   "-- STANDARD PARSER CODE",
   "-- Types",
   "data Leaf = T Token String | NT NonTerminal",
   "   deriving (Show)",
   "type TokenTuple = (Token, String)",
   "type ParseTree = Tree Leaf",
   "-- The state contains respectively: ",
   "-- (whether parsing is still succesful, parsed tokens, tokens still to be parsed, current parse tree)",
   "type State = (Bool, [TokenTuple], [TokenTuple], ParseTree)",
   "",
   "-- Standard code",
   "class Symbol s where",
   "   parseEOF :: s -> Bool",
   "   parseRule :: s -> Token -> (State -> State)",
   "",
   "-- Tries to parse the current state with the given non-terminal",
   "parse :: NonTerminal -> State -> State",
   "parse s (True, c:tokens, accepted, (Node nt children)) = (newStatus, newTokens, newAccepted, Node nt (children++[subTree]))",
   "   where",
   "      (newStatus, newTokens, newAccepted, subTree) = (parseRule s (fst c)) (True, c:tokens, accepted, Node (NT s) [])",
   "parse s (True, [], accepted, tree) = (parseEOF s, [], accepted, tree) -- If the list of accepted tokens is empty, we must accept EOF",
   "parse _ state = parseFailure state",
   "",
   "-- Tries to parse the current state with the given terminal",
   "parseToken :: Token -> State -> State",
   "parseToken x (True, (y,s):t, a, Node nt children)",
   "             | x == y    = (True, t, a++[(x,s)], Node nt (children++[Node (T y s) []]))",
   "             | otherwise = (False, (y,s):t, a, Node nt children)",
   "parseToken _ (False, t, a, tree)   = (False, t, a, tree)  -- If parsing already failed nothing changes",
   "parseToken _ (True, [], a, tree)   = (False, [], a, tree) -- If we are already done, this token is unexpected",
   "",
   "-- Does not nothing, just sets the status to False",
   "parseFailure :: State -> State",
   "parseFailure (_, tokens, accepted, tree) = (False, tokens, accepted, tree)",
   "",
   "-- Does nothing (accepts epsilon)",
   "parseEpsilon :: State -> State",
   "parseEpsilon x = x",
   "",
   "-- Functions to be exported",
   "_parser :: NonTerminal -> [TokenTuple] -> (Bool, [TokenTuple], [TokenTuple])",
   "_parser s t = (status && (tokens == []), accepted, tokens)",
   "   where",
   "      (status, tokens, accepted, (Node x [tree])) = parse s (True, t, [], Node (NT s) [])",
   "",
   "_parseTree :: NonTerminal -> [TokenTuple] -> ParseTree",
   "_parseTree s t",
   "   | status && (tokens == []) = tree",
   "   | otherwise = trace \"*** Warning! The string was not accepted by the parser.\" tree",
   "   where",
   "      (status, tokens, accepted, (Node x [tree])) = parse s (True, t, [], Node (NT s) [])",
   "",
   "printParseTree :: [TokenTuple] -> IO ()",
   "printParseTree = putStr.drawTree.toStringTree.parseTree",
   "   where",
   "      toStringTree :: ParseTree -> Tree String",
   "      toStringTree (Node (NT s) children) = (Node (show s) (map toStringTree children))",
   "      toStringTree (Node (T t s) children) = (Node s [])",
   ""])
