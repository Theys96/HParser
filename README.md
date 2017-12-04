# HParser
A parser generator in Haskell for Haskell.

## 1. Write a grammar
```haskell
import HParser.Grammar
import HParser.Generator

grammar = Grammar [
   Rule (NonTerminal "S") [NonTerminal "E", NonTerminal "Sp"],
   Rule (NonTerminal "Sp") [],
   Rule (NonTerminal "Sp") [Terminal "+", NonTerminal "S"],
   Rule (NonTerminal "E") [Terminal "1"],
   Rule (NonTerminal "E") [Terminal "(", NonTerminal "S", Terminal ")"]
   ]
```

## 2. Generate a parser from it
```bash
$ ghci
Prelude> :load Example
*Main> writeFile "TestParser.hs" (genParser grammar)
```

## Disclaimer
This piece of software is in an extremely early stage of development and should not be used in real-life yet.