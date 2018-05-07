module HParser.Parser (
   (>>>),
   trace,
   ParseTree (..),
   Tree (..),
   Symbol (..),
   TokenTuple (..),
   parseToken,
   parseFailure,
   parseEpsilon,
   printParseTree,
   parse
   ) where

import Data.Tree
import Debug.Trace
import Control.Arrow

data Token = TOKEN
   deriving (Read, Show, Enum, Eq, Ord)
type TokenTuple = (Token, String)

type ParseTree = Tree String
class Symbol s where
   parseEOF :: s -> Bool
   parseRule :: s -> Token -> ((Bool, [TokenTuple], [TokenTuple], ParseTree) -> (Bool, [TokenTuple], [TokenTuple], ParseTree))

parseToken :: TokenTuple -> (Bool, [TokenTuple], [TokenTuple], ParseTree) -> (Bool, [TokenTuple], [TokenTuple], ParseTree)
parseToken x (True, y:t, a, Node nt children)
                          | x == y    = (True, t, a++[x], Node nt (children++[Node (show y) []]))
                          | otherwise = (False, y:t, a, Node nt children)
parseToken _ (False, t, a, tree)   = (False, t, a, tree)
parseToken _ (True, [], a, tree)   = (False, [], a, tree)

parseFailure :: (Bool, [TokenTuple], [TokenTuple], ParseTree) -> (Bool, [TokenTuple], [TokenTuple], ParseTree)
parseFailure (_, tokens, accepted, tree) = (False, tokens, accepted, tree)

parseEpsilon :: (Bool, [TokenTuple], [TokenTuple], ParseTree) -> (Bool, [TokenTuple], [TokenTuple], ParseTree)
parseEpsilon x = x

printParseTree :: ParseTree -> IO ()
printParseTree = putStr.drawTree

parse :: (Symbol s, Show s) => s -> (Bool, [TokenTuple], [TokenTuple], ParseTree) -> (Bool, [TokenTuple], [TokenTuple], ParseTree)
parse s (True, c:tokens, accepted, (Node nt children)) = (newStatus, newTokens, newAccepted, Node nt (children++[subTree]))
   where
      (newStatus, newTokens, newAccepted, subTree) = (parseRule s (fst c)) (True, c:tokens, accepted, Node (show s) [])
parse s (True, [], accepted, tree) = (parseEOF s, [], accepted, tree)
parse _ x = parseFailure x
