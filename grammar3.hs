import HParser.Grammar
import HParser.Generator

grammar = Grammar [
   Rule (NonTerminal "S") [(NonTerminal "E"), (NonTerminal "S")],
   Rule (NonTerminal "S") [],
   Rule (NonTerminal "E") [(Terminal "e")],
   Rule (NonTerminal "E") [(Terminal "k")]
   ]
