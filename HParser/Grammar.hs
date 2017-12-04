module HParser.Grammar (
   Symbol (..), Rule (..), Grammar (..), nonTerminalNames
   ) where

import Data.List
import qualified Data.Set as Set

data Symbol = Terminal String | NonTerminal String | Epsilon
data Rule = Rule Symbol [Symbol]
data Grammar = Grammar [Rule]

instance Show Symbol where
   show (NonTerminal c) = c
   show (Terminal c) = ("'"++c++"'")
   show Epsilon = "\949"

instance Show Rule where
   show (Rule start []) = (show start) ++ " \t-> " ++ (show Epsilon)
   show (Rule start tokens) = (show start) ++ " \t-> " ++ (intercalate " " $ map show $ tokens)

instance Show Grammar where
   show (Grammar rules) = (intercalate "\n" $ map show $ rules)

instance Eq Symbol where
   (==) (NonTerminal a) (NonTerminal b) = a == b
   (==) (Terminal a) (Terminal b) = a == b
   (==) Epsilon Epsilon = True
   (==) _ _ = False

instance Ord Symbol where
   (NonTerminal nt1) `compare` (NonTerminal nt2) = nt1 `compare` nt2
   (Terminal t1) `compare` (Terminal t2) = t1 `compare` t2
   Epsilon `compare` _ = LT
   _ `compare` Epsilon = GT
   (Terminal _) `compare` (NonTerminal _) = LT
   (NonTerminal _) `compare` (Terminal _) = GT

nonTerminalNames :: Grammar -> [String]
nonTerminalNames (Grammar rules) = nub [name | Rule (NonTerminal name) rh <- rules]
