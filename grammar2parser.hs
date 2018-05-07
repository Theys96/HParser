
import HParser.Parser

data NonTerminal = S | A | B

instance Show NonTerminal where
   show S = "S"
   show A = "A"
   show B = "B"

instance Symbol NonTerminal where
   parseEOF A = True
   parseEOF B = True
   parseEOF _ = False

   parseRule S 'a' = parseToken 'a' >>> parse A
   parseRule S 'b' = parseToken 'b' >>> parse B
   parseRule A 'a' = parseToken 'a'
   parseRule B 'b' = parseToken 'b'
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

