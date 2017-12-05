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

```haskell
parse :: NonTerminal -> (Bool, [Char], [Char]) -> (Bool, [Char], [Char])
parse _ (False, ts, a) = (False, ts, a)
parse S (True, '(':tokens, accepted) = parse Sp $ parse E $ (True, '(':tokens, accepted)
parse S (True, '1':tokens, accepted) = parse Sp $ parse E $ (True, '1':tokens, accepted)
parse Sp (True, ')':tokens, accepted) = (True, ')':tokens, accepted)
parse Sp (True, [], accepted) =  (True, [], accepted)
parse Sp (True, '+':tokens, accepted) = parse S $ parseToken '+' $ (True, '+':tokens, accepted)
parse E (True, '1':tokens, accepted) = parseToken '1' $ (True, '1':tokens, accepted)
parse E (True, '(':tokens, accepted) = parseToken ')' $ parse S $ parseToken '(' $ (True, '(':tokens, accepted)
parse _ (_, ts, a) = (False, ts, a)
```

## Disclaimer
This piece of software is in an extremely early stage of development and should not be used in real-life yet.