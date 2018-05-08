module Grammar5 (grammar5) where

import HParser.Grammar

grammar5 = Grammar (NonTerminal "S") [
   Rule (NonTerminal "S") [(NonTerminal "E")],
   Rule (NonTerminal "S") [(Terminal "a")],
   Rule (NonTerminal "E") [(Terminal "a")],
   Rule (NonTerminal "E") []
   ]
