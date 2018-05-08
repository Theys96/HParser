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

## 3. Check it and generate a parser from it
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

## 4. Load the parser and test it
```bash
*Main> :load TestParser
[1 of 2] Compiling HParser.Parser   ( HParser/Parser.hs, interpreted )
[2 of 2] Compiling Main             ( TestParser.hs, interpreted )
Ok, 2 modules loaded.
*Main> parser "1+1+1"
(True,"1+1+1","")
*Main> parser "(1+1)+(1+1)"
(True,"(1+1)+(1+1)","")
*Main> parser "(1+1)(1+1)"
(False,"(1+1)","(1+1)")
*Main> parser ""
(False,"","")
*Main> printParseTree $ parseTree "(1+1)+1"
|
`- S
   |
   +- E
   |  |
   |  +- '('
   |  |
   |  +- S
   |  |  |
   |  |  +- E
   |  |  |  |
   |  |  |  `- '1'
   |  |  |
   |  |  `- Sp
   |  |     |
   |  |     +- '+'
   |  |     |
   |  |     `- S
   |  |        |
   |  |        +- E
   |  |        |  |
   |  |        |  `- '1'
   |  |        |
   |  |        `- Sp
   |  |
   |  `- ')'
   |
   `- Sp
      |
      +- '+'
      |
      `- S
         |
         `- E
            |
            `- '1'
```

## Disclaimer
This piece of software is in an extremely early stage of development and should not be used in real-life yet.