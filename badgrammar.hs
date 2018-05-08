import HParser.Grammar
import HParser.Generator

grammar = Grammar (Terminal "Token") [
   Rule (NonTerminal "S") [(Terminal "Token")],
   Rule (Terminal "Token") []
   ]

