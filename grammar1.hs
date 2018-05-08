import HParser.Grammar
import HParser.Generator

grammar = Grammar (NonTerminal "S") [
   Rule (NonTerminal "S") [(NonTerminal "E"), (NonTerminal "Sp")],
   Rule (NonTerminal "Sp") [],
   Rule (NonTerminal "Sp") [(Terminal "PLUS"), (NonTerminal "S")],
   Rule (NonTerminal "E") [(Terminal "ONE")],
   Rule (NonTerminal "E") [(Terminal "OPEN"), (NonTerminal "S"), (Terminal "CLOSE")]
   ]



