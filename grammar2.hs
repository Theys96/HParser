import HParser.Grammar
import HParser.Generator

-- Optional:
import HParser.FirstSet
import HParser.FollowSet

grammar = Grammar [
   Rule (NonTerminal "S") [Terminal "a", NonTerminal "A"],
   Rule (NonTerminal "S") [Terminal "b", NonTerminal "B"],
   Rule (NonTerminal "A") [Terminal "a"],
   Rule (NonTerminal "A") [],
   Rule (NonTerminal "B") [Terminal "b"],
   Rule (NonTerminal "B") []
   ]

-- To generate this, load in this file and call "genParser grammar"
-- This returns a string containing the parser code
-- To save to a file: writeFile "parser.hs" $ genParser grammar
-- The resulting code can immediatly be run and does not use 
-- the HParser modules