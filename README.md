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

## 2. Check it and generate a parser from it
```bash
$ ghci
Prelude> :load Example
*Main> grammar
S 	-> E Sp
Sp 	-> ε
Sp 	-> '+' S
E 	-> '1'
E 	-> '(' S ')'
*Main> saveParser "TestParser.hs" grammar
```

Example snippet:
```haskell
instance Symbol NonTerminal where
   parseEOF Sp = True
   parseEOF _ = False

   parseRule S '(' = parse E >>> parse Sp
   parseRule S '1' = parse E >>> parse Sp
   parseRule Sp ')' = parseEpsilon
   parseRule Sp '+' = parseToken '+' >>> parse S
   parseRule E '1' = parseToken '1'
   parseRule E '(' = parseToken '(' >>> parse S >>> parseToken ')'
   parseRule _ _ = parseFailure
```

## Disclaimer
This piece of software is in an extremely early stage of development and should not be used in real-life yet.