module Grammar1 (grammar1) where

import HParser.Grammar
import HParser.Generator

grammar1 = Grammar (NonTerminal "S") [
	Rule (NonTerminal "S") [NonTerminal "E", NonTerminal "Sp"],
	Rule (NonTerminal "Sp") [],
	Rule (NonTerminal "Sp") [Terminal "PLUS", NonTerminal "S"],
	Rule (NonTerminal "E") [Terminal "ONE"],
	Rule (NonTerminal "E") [Terminal "OPEN", NonTerminal "S", Terminal "CLOSE"]
]
