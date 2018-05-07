import HParser.Grammar
import HParser.Generator

grammar = Grammar [
   Rule (NonTerminal "S") [(NonTerminal "E"), (NonTerminal "Sp")],
   Rule (NonTerminal "Sp") [],
   Rule (NonTerminal "Sp") [(Terminal "+"), (NonTerminal "S")],
   Rule (NonTerminal "E") [(Terminal "1")],
   Rule (NonTerminal "E") [(Terminal "("), (NonTerminal "S"), (Terminal ")")]
   ]
