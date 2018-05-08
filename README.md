[![Build Status](https://travis-ci.org/Theys96/HParser.svg?branch=master)](https://travis-ci.org/Theys96/HParser)

[Module reference](https://htmlpreview.github.io/?https://raw.githubusercontent.com/Theys96/HParser/develop/docs/index.html)

# HParser
A parser generator in Haskell (and a bit of C) for Haskell.

## 1. First, make everything
```
$ make
```

## 2. Write a grammar
```
S : E Sp;

Sp :
   | '+' S
   ;

E : '1'
  | '(' S ')'
  ;
```

## 3. Convert it
```bash
$ grammars/HGrammar grammarFile grammar1 S
```
The resulting grammar is in Haskell:
```haskell
module Grammar1 (grammar1) where

import HParser.Grammar

grammar1 = Grammar (NonTerminal "S") [
   Rule (NonTerminal "S") [(NonTerminal "E"), (NonTerminal "Sp")],
   Rule (NonTerminal "Sp") [],
   Rule (NonTerminal "Sp") [(Terminal "+"), (NonTerminal "S")],
   Rule (NonTerminal "E") [(Terminal "1")],
   Rule (NonTerminal "E") [(Terminal "("), (NonTerminal "S"), (Terminal ")")]
   ]
```

## 4. Generate a parser from it
```
$ ghci grammar
GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
[1 of 5] Compiling HParser.Grammar  ( HParser/Grammar.hs, interpreted )
[2 of 5] Compiling HParser.FirstSet ( HParser/FirstSet.hs, interpreted )
[3 of 5] Compiling HParser.FollowSet ( HParser/FollowSet.hs, interpreted )
[4 of 5] Compiling HParser.Generator ( HParser/Generator.hs, interpreted )
[5 of 5] Compiling Grammar          ( grammar.hs, interpreted )
Ok, 5 modules loaded.
*Grammar> grammar
S   -> E Sp
Sp  -> ε
Sp  -> '+' S
E   -> '1'
E   -> '(' S ')'
*Grammar> putStr $ genParser grammar "Parser"
module Parser (Token (..), TokenTuple (..), ParseTree (..), parser, parseTree, printParseTree) where

import Data.Tree
import Debug.Trace
import Control.Arrow

-- GRAMMAR-SPECIFIC PARSER CODE
data Token = + | 1 | ( | )
   deriving (Read, Show, Eq)

data NonTerminal = S | Sp | E
   deriving (Read, Show, Eq)

instance Symbol NonTerminal where
   parseEOF Sp = True
   parseEOF _ = False

   parseRule S ( = parse E >>> parse Sp
   parseRule S 1 = parse E >>> parse Sp
   parseRule Sp ) = parseEpsilon
   parseRule Sp + = parseToken + >>> parse S
   parseRule E 1 = parseToken 1
   parseRule E ( = parseToken ( >>> parse S >>> parseToken )
   parseRule _ _ = parseFailure

-- Set starting symbol
parser = _parser S
parseTree = _parseTree S


(Rest of standard parser code omitted)
```

## Disclaimer
This piece of software is in an extremely early stage of development and should not be used in real-life yet.