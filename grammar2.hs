module Grammar2 (grammar2) where

import HParser.Grammar
import HParser.Generator

grammar2 = Grammar (NonTerminal "S") [
   Rule (NonTerminal "S") [NonTerminal "A", Terminal "a", NonTerminal "A", Terminal "b"],
   Rule (NonTerminal "S") [NonTerminal "B", Terminal "b", NonTerminal "B", Terminal "a"],
   Rule (NonTerminal "A") [],
   Rule (NonTerminal "B") []
   ]
