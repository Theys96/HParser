
import HParser.Parser

data NonTerminal = S | E

instance Show NonTerminal where
   show S = "S"
   show E = "E"

instance Symbol NonTerminal where
   parseEOF S = True
   parseEOF _ = False

   parseRule S 'e' = parse E >>> parse S
   parseRule S 'k' = parse E >>> parse S
   parseRule E 'e' = parseToken 'e'
   parseRule E 'k' = parseToken 'k'
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

