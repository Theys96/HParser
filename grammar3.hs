module Grammar3 (grammar3) where

import HParser.Grammar
import HParser.Generator

grammar3 = Grammar (NonTerminal "S") [
	Rule (NonTerminal "S") [NonTerminal "T", NonTerminal "A"]
	Rule (NonTerminal "A") [Terminal "PLUS", NonTerminal "T", NonTerminal "A"]
	Rule (NonTerminal "A") []
	Rule (NonTerminal "T") [NonTerminal "F", NonTerminal "B"]
	Rule (NonTerminal "B") [Terminal "PRODUCT", NonTerminal "F", NonTerminal "B"]
	Rule (NonTerminal "B") []
	Rule (NonTerminal "F") [Terminal "OPEN", NonTerminal "S", Terminal "CLOSE"]
	Rule (NonTerminal "F") [Terminal "x"]
]
