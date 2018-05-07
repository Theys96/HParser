import HParser.Grammar
import HParser.Generator

-- Optional:
import HParser.FirstSet
import HParser.FollowSet

grammar = Grammar [
   Rule (NonTerminal "S") [NonTerminal "E", NonTerminal "Sp"],
   Rule (NonTerminal "Sp") [],
   Rule (NonTerminal "Sp") [Terminal "+", NonTerminal "S"],
   Rule (NonTerminal "E") [Terminal "1"],
   Rule (NonTerminal "E") [Terminal "(", NonTerminal "S", Terminal ")"]
   ]

-- To generate this, load in this file and call "genParser grammar"
-- This returns a string containing the parser code
-- To save to a file: writeFile "parser.hs" $ genParser grammar
-- The resulting code can immediatly be run and does not use 
-- the HParser modules