[![Build Status](https://travis-ci.org/Theys96/HParser.svg?branch=master)](https://travis-ci.org/Theys96/HParser)

[Module reference](https://htmlpreview.github.io/?https://raw.githubusercontent.com/Theys96/HParser/develop/docs/index.html)

# HParser
A parser generator in Haskell (and a bit of C) for Haskell.

## 1. First, make everything
```
$ make
$ make clean
```

## 2. Write a grammar
```
%tokens PLUS OPEN CLOSE ONE
%start S

%%

S : E Sp;

Sp : /* epsilon */
   | PLUS S
   ;

E : ONE
  | OPEN S CLOSE
  ;
```

## 3. Convert it
```bash
$ HGrammar/HGrammar grammarFile grammar1
```
The resulting grammar is in Haskell:
```haskell
module Grammar1 (grammar1) where

import HParser.Grammar
import HParser.Generator

grammar1 = Grammar (NonTerminal "S") [
   Rule (NonTerminal "S") [NonTerminal "E", NonTerminal "Sp"],
   Rule (NonTerminal "Sp") [],
   Rule (NonTerminal "Sp") [Terminal "PLUS", NonTerminal "S"],
   Rule (NonTerminal "E") [Terminal "ONE"],
   Rule (NonTerminal "E") [Terminal "OPEN", NonTerminal "S", Terminal "CLOSE"]
   ]
```

## 4. Generate a parser from it
```
ThijsMac:HParser thijs$ ghci grammar1
GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
[1 of 5] Compiling HParser.Grammar  ( HParser/Grammar.hs, interpreted )
[2 of 5] Compiling HParser.FirstSet ( HParser/FirstSet.hs, interpreted )
[3 of 5] Compiling HParser.FollowSet ( HParser/FollowSet.hs, interpreted )
[4 of 5] Compiling HParser.Generator ( HParser/Generator.hs, interpreted )
[5 of 5] Compiling Grammar1         ( grammar1.hs, interpreted )
Ok, 5 modules loaded.
*Grammar1> grammar1
S  -> E Sp
Sp    -> ε
Sp    -> 'PLUS' S
E  -> 'ONE'
E  -> 'OPEN' S 'CLOSE'
*Grammar1> -- Generate a parser and put it to the console
*Grammar1> putStr $ genParser grammar1 "Parser"
module Parser (Token (..), TokenTuple (..), ParseTree (..), parser, parseTree, printParseTree) where

import Data.Tree
import Debug.Trace
import Control.Arrow

-- GRAMMAR-SPECIFIC PARSER CODE
data Token = PLUS | ONE | OPEN | CLOSE
   deriving (Read, Show, Eq)

data NonTerminal = S | Sp | E
   deriving (Read, Show, Eq)

instance Symbol NonTerminal where
   parseEOF Sp = True
   parseEOF _ = False

   parseRule S ONE = parse E >>> parse Sp
   parseRule S OPEN = parse E >>> parse Sp
   parseRule Sp CLOSE = parseEpsilon
   parseRule Sp PLUS = parseToken PLUS >>> parse S
   parseRule E ONE = parseToken ONE
   parseRule E OPEN = parseToken OPEN >>> parse S >>> parseToken CLOSE
   parseRule _ _ = parseFailure

-- Set starting symbol
parser = _parser S
parseTree = _parseTree S


(Rest of standard parser code omitted)
```

## 5. Save your parser as a module
```
*Grammar1> saveParser grammar1 "Grammar1Parser"
*Grammar1> :load Grammar1Parser
[1 of 1] Compiling Grammar1Parser   ( Grammar1Parser.hs, interpreted )
Ok, 1 module loaded.
```

## 6. Use the parser
```
*Grammar1Parser> lexedInput = [(OPEN,"("),(ONE,"1"),(CLOSE,")"),(PLUS,"+"),(ONE,"1")]
*Grammar1Parser> parser lexedInput
(True,[(OPEN,"("),(ONE,"1"),(CLOSE,")"),(PLUS,"+"),(ONE,"1")],[])
*Grammar1Parser> -- Output: success value (True), accepted tokens (all of them), unaccepted tokens (none)
*Grammar1Parser> printParseTree lexedInput
S
|
+- E
|  |
|  +- (
|  |
|  +- S
|  |  |
|  |  +- E
|  |  |  |
|  |  |  `- 1
|  |  |
|  |  `- Sp
|  |
|  `- )
|
`- Sp
   |
   +- +
   |
   `- S
      |
      `- E
         |
         `- 1
```

## Disclaimer
This piece of software is in an early stage of development.