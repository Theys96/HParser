
import HParser.Parser
import System.Environment

data NonTerminal = S | Sp | E

instance Show NonTerminal where
   show S = "S"
   show Sp = "Sp"
   show E = "E"

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

parser :: [Char] -> (Bool, [Char], [Char])
parser t = (status && (tokens == []), accepted, tokens)
   where
      (status, tokens, accepted, tree) = parse S (True, t, [], Node "" [])

parseTree :: [Char] -> ParseTree
parseTree t
   | status && (tokens == []) = tree
   | otherwise = trace "*** Warning! The string was not accepted by the parser.\n" tree
   where
      (status, tokens, accepted, tree) = parse S (True, t, [], Node "" [])

