module Grammar (grammar) where

import HParser.Grammar
import HParser.Generator

grammar = Grammar (NonTerminal "Expression") [
   Rule (NonTerminal "S") [NonTerminal "E", NonTerminal "Sp"],
   Rule (NonTerminal "Sp") [],
   Rule (NonTerminal "Sp") [Terminal "PLUS", NonTerminal "S"],
   Rule (NonTerminal "E") [Terminal "NUM"],
   Rule (NonTerminal "E") [Terminal "OPEN", NonTerminal "S", Terminal "CLOSE"]
   ]
