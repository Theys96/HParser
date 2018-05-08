[![Build Status](https://travis-ci.org/Theys96/HParser.svg?branch=master)](https://travis-ci.org/Theys96/HParser)

[Module reference](https://htmlpreview.github.io/?https://raw.githubusercontent.com/Theys96/HParser/develop/docs/index.html)

# HParser
A parser generator in Haskell (and a bit of C) for Haskell.

## 1. Write a grammar
```
S : E Sp;

Sp :
   | '+' S
   ;

E : '1'
  | '(' S ')'
  ;
```

## 2. Convert it
```bash
$ cd grammars
$ make
cc -O2 -Wall   -c -o grammars.o grammars.c
gcc grammars.o -o HGrammar
$ cd ..
$ grammars/HGrammar grammar
```
The resulting grammar is in Haskell:
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

## Disclaimer
This piece of software is in an extremely early stage of development and should not be used in real-life yet.