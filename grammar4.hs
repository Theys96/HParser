module Grammar4 (grammar4) where

import HParser.Grammar
import HParser.Generator

-- This is a corrupt grammar for testing
grammar4 = Grammar (Terminal "a") [
   Rule (Terminal "a") [Terminal "aa"]
   ]