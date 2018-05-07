module NewParser (
   Token (..),
   TokenTuple (..),
   ParseTree (..),
   parser,
   parseTree,
   printParseTree
   ) where

import Data.Tree
import Debug.Trace
import Control.Arrow

-- GRAMMAR-SPECIFIC PARSER CODE
data Token = BRKTOPEN | ONE | BRKTCLOSE | PLUS
   deriving (Read, Show, Enum, Eq, Ord)

data NonTerminal = S | Sp | E
   deriving (Read, Show, Enum, Eq, Ord)

data Leaf = T Token String | NT NonTerminal
   deriving (Show)

instance Symbol NonTerminal where
   parseEOF Sp = True
   parseEOF _ = False

   parseRule S BRKTOPEN = parse E >>> parse Sp
   parseRule S ONE = parse E >>> parse Sp
   parseRule Sp BRKTCLOSE = parseEpsilon
   parseRule Sp PLUS = parseToken PLUS >>> parse S
   parseRule E ONE = parseToken ONE
   parseRule E BRKTOPEN = parseToken BRKTOPEN >>> parse S >>> parseToken BRKTCLOSE
   parseRule _ _ = parseFailure


-- STANDARD PARSER CODE

-- Types
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


-- User code
parser :: [TokenTuple] -> (Bool, [TokenTuple], [TokenTuple])
parser t = (status && (tokens == []), accepted, tokens)
   where
      (status, tokens, accepted, (Node x [tree])) = parse S (True, t, [], Node (NT S) [])

parseTree :: [TokenTuple] -> ParseTree
parseTree t
   | status && (tokens == []) = tree
   | otherwise = trace "*** Warning! The string was not accepted by the parser.\n" tree
   where
      (status, tokens, accepted, (Node x [tree])) = parse S (True, t, [], Node (NT S) [])

toStringTree :: ParseTree -> Tree String
toStringTree (Node (NT s) children) = (Node (show s) (map toStringTree children))
toStringTree (Node (T t s) children) = (Node s [])

printParseTree :: [TokenTuple] -> IO ()
printParseTree = putStr.drawTree.toStringTree.parseTree
