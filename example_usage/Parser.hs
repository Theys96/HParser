module Parser (Token (..), NonTerminal (..), TokenTuple (..), Leaf (..), ParseTree (..), parser, parseTree, printParseTree) where

import Data.Tree
import Debug.Trace
import Control.Arrow

-- GRAMMAR-SPECIFIC PARSER CODE
data Token = PLUS | NUM | OPEN | CLOSE
   deriving (Read, Show, Eq)

data NonTerminal = S | Sp | E
   deriving (Read, Show, Eq)

instance Symbol NonTerminal where
   parseEOF Sp = True
   parseEOF _ = False

   parseRule S NUM = parse E >>> parse Sp
   parseRule S OPEN = parse E >>> parse Sp
   parseRule Sp CLOSE = parseEpsilon
   parseRule Sp PLUS = parseToken PLUS >>> parse S
   parseRule E NUM = parseToken NUM
   parseRule E OPEN = parseToken OPEN >>> parse S >>> parseToken CLOSE
   parseRule _ _ = parseFailure

-- Set starting symbol
parser = _parser S
parseTree = _parseTree S




-- STANDARD PARSER CODE
-- Types
data Leaf = T Token String | NT NonTerminal
   deriving (Show)
type TokenTuple = (Token, String)
type ParseTree = Tree Leaf
-- The state contains respectively: 
-- (whether parsing is still succesful, parsed tokens, tokens still to be parsed, current parse tree)
type State = (Bool, [TokenTuple], [TokenTuple], ParseTree)

-- Standard code
class Symbol s where
   parseEOF :: s -> Bool
   parseRule :: s -> Token -> (State -> State)

-- Tries to parse the current state with the given non-terminal
parse :: NonTerminal -> State -> State
parse s (True, c:tokens, accepted, (Node nt children)) = (newStatus, newTokens, newAccepted, Node nt (children++[subTree]))
   where
      (newStatus, newTokens, newAccepted, subTree) = (parseRule s (fst c)) (True, c:tokens, accepted, Node (NT s) [])
parse s (True, [], accepted, tree) = (parseEOF s, [], accepted, tree) -- If the list of accepted tokens is empty, we must accept EOF
parse _ state = parseFailure state

-- Tries to parse the current state with the given terminal
parseToken :: Token -> State -> State
parseToken x (True, (y,s):t, a, Node nt children)
             | x == y    = (True, t, a++[(x,s)], Node nt (children++[Node (T y s) []]))
             | otherwise = (False, (y,s):t, a, Node nt children)
parseToken _ (False, t, a, tree)   = (False, t, a, tree)  -- If parsing already failed nothing changes
parseToken _ (True, [], a, tree)   = (False, [], a, tree) -- If we are already done, this token is unexpected

-- Does not nothing, just sets the status to False
parseFailure :: State -> State
parseFailure (_, tokens, accepted, tree) = (False, tokens, accepted, tree)

-- Does nothing (accepts epsilon)
parseEpsilon :: State -> State
parseEpsilon x = x

-- Functions to be exported
_parser :: NonTerminal -> [TokenTuple] -> (Bool, [TokenTuple], [TokenTuple])
_parser s t = (status && (tokens == []), accepted, tokens)
   where
      (status, tokens, accepted, (Node x [tree])) = parse s (True, t, [], Node (NT s) [])

_parseTree :: NonTerminal -> [TokenTuple] -> ParseTree
_parseTree s t
   | status && (tokens == []) = tree
   | otherwise = trace "*** Warning! The string was not accepted by the parser." tree
   where
      (status, tokens, accepted, (Node x [tree])) = parse s (True, t, [], Node (NT s) [])

printParseTree :: [TokenTuple] -> IO ()
printParseTree = putStr.drawTree.toStringTree.parseTree
   where
      toStringTree :: ParseTree -> Tree String
      toStringTree (Node (NT s) children) = (Node (show s) (map toStringTree children))
      toStringTree (Node (T t s) children) = (Node s [])
