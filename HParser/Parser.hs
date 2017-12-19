module HParser.Parser (
   (>>>),
   trace,
   ParseTree (..),
   Tree (..),
   Symbol (..),
   parseToken,
   parseFailure,
   parseEpsilon,
   printParseTree,
   parse
   ) where

import Data.Tree
import Debug.Trace
import Control.Arrow

type ParseTree = Tree String
class Symbol s where
   parseEOF :: s -> Bool
   parseRule :: s -> Char -> ((Bool, [Char], [Char], ParseTree) -> (Bool, [Char], [Char], ParseTree))

parseToken :: Char -> (Bool, [Char], [Char], ParseTree) -> (Bool, [Char], [Char], ParseTree)
parseToken x (True, y:t, a, Node nt children)
                          | x == y    = (True, t, a++[x], Node nt (children++[Node (show y) []]))
                          | otherwise = (False, y:t, a, Node nt children)
parseToken _ (False, t, a, tree)   = (False, t, a, tree)
parseToken _ (True, [], a, tree)   = (False, [], a, tree)

parseFailure :: (Bool, [Char], [Char], ParseTree) -> (Bool, [Char], [Char], ParseTree)
parseFailure (_, tokens, accepted, tree) = (False, tokens, accepted, tree)

parseEpsilon :: (Bool, [Char], [Char], ParseTree) -> (Bool, [Char], [Char], ParseTree)
parseEpsilon x = x

printParseTree :: ParseTree -> IO ()
printParseTree = putStr.drawTree

parse :: (Symbol s, Show s) => s -> (Bool, [Char], [Char], ParseTree) -> (Bool, [Char], [Char], ParseTree)
parse s (True, c:tokens, accepted, (Node nt children)) = (newStatus, newTokens, newAccepted, Node nt (children++[subTree]))
   where
      (newStatus, newTokens, newAccepted, subTree) = (parseRule s c) (True, c:tokens, accepted, Node (show s) [])
parse s (True, [], accepted, tree) = (parseEOF s, [], accepted, tree)
parse _ x = parseFailure x