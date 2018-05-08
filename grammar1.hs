module Grammar1 (grammar1) where

import HParser.Grammar

grammar1 = Grammar (NonTerminal "S") [
   Rule (NonTerminal "S") [(NonTerminal "E"), (NonTerminal "Sp")],
   Rule (NonTerminal "Sp") [],
   Rule (NonTerminal "Sp") [(Terminal "+"), (NonTerminal "S")],
   Rule (NonTerminal "E") [(Terminal "1")],
   Rule (NonTerminal "E") [(Terminal "("), (NonTerminal "S"), (Terminal ")")]
   ]
