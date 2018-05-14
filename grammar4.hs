module Grammar4 (grammar4) where

import HParser.Grammar
import HParser.Generator

grammar4 = Grammar (NonTerminal "a") [
   Rule (Terminal "a") [Terminal "aa"]
]