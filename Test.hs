import HParser.Grammar
import HParser.Generator
import HParser.FirstSet
import HParser.FollowSet

import qualified Data.Set as S
import Data.List
import Test.HUnit

-- NOTE: You must install HUnit on your system to run the tests

grammar1 = Grammar [
   Rule (NonTerminal "S") [NonTerminal "E", NonTerminal "Sp"],
   Rule (NonTerminal "Sp") [],
   Rule (NonTerminal "Sp") [Terminal "+", NonTerminal "S"],
   Rule (NonTerminal "E") [Terminal "1"],
   Rule (NonTerminal "E") [Terminal "(", NonTerminal "S", Terminal ")"]
   ]

grammar2 = Grammar [
   Rule (NonTerminal "S") [NonTerminal "A", Terminal "a", NonTerminal "A", Terminal "b"],
   Rule (NonTerminal "S") [NonTerminal "B", Terminal "b", NonTerminal "B", Terminal "a"],
   Rule (NonTerminal "A") [],
   Rule (NonTerminal "B") []
   ]

grammar3 = Grammar [
   Rule (NonTerminal "S") [NonTerminal "T", NonTerminal "A"],
   Rule (NonTerminal "A") [Terminal "+", NonTerminal "T", NonTerminal "A"],
   Rule (NonTerminal "A") [],
   Rule (NonTerminal "T") [NonTerminal "F", NonTerminal "B"],
   Rule (NonTerminal "B") [Terminal "*", NonTerminal "F", NonTerminal "B"],
   Rule (NonTerminal "B") [],
   Rule (NonTerminal "F") [Terminal "(", NonTerminal "S", Terminal ")"],
   Rule (NonTerminal "F") [Terminal "x"]
   ]

-- The first and follow set tests are done with http://mdaines.github.io/grammophone/

test1 = TestCase (
   assertEqual "First set of S in grammar 1"
      (sort ["1", "("])
      (S.toList $ symbolFirstSet grammar1 (NonTerminal "S"))
   )
test2 = TestCase (
   assertEqual "First set of Sp in grammar 1"
      (sort ["+",""])
      (S.toList $ symbolFirstSet grammar1 (NonTerminal "Sp"))
   )
test3 = TestCase (
   assertEqual "First set of E in grammar 1"
      (sort ["1","("])
      (S.toList $ symbolFirstSet grammar1 (NonTerminal "E"))
   )

test4 = TestCase (
   assertEqual "Follow set of S in grammar 1"
      (sort [")", ""])
      (S.toList $ followSet grammar1 (NonTerminal "S"))
   )
test5 = TestCase (
   assertEqual "Follow set of Sp in grammar 1"
      (sort [")",""])
      (S.toList $ followSet grammar1 (NonTerminal "Sp"))
   )
test6 = TestCase (
   assertEqual "Follow set of E in grammar 1"
      (sort ["+",")",""])
      (S.toList $ followSet grammar1 (NonTerminal "E"))
   )


test7 = TestCase (
   assertEqual "First set of S in grammar 2"
      (sort ["a", "b"])
      (S.toList $ symbolFirstSet grammar2 (NonTerminal "S"))
   )
test8 = TestCase (
   assertEqual "First set of A in grammar 2"
      (sort [""])
      (S.toList $ symbolFirstSet grammar2 (NonTerminal "A"))
   )
test9 = TestCase (
   assertEqual "First set of B in grammar 2"
      (sort [""])
      (S.toList $ symbolFirstSet grammar2 (NonTerminal "B"))
   )

test10 = TestCase (
   assertEqual "Follow set of S in grammar 2"
      (sort [""])
      (S.toList $ followSet grammar2 (NonTerminal "S"))
   )
test11 = TestCase (
   assertEqual "Follow set of A in grammar 2"
      (sort ["a","b"])
      (S.toList $ followSet grammar2 (NonTerminal "A"))
   )
test12 = TestCase (
   assertEqual "Follow set of B in grammar 2"
      (sort ["a","b"])
      (S.toList $ followSet grammar2 (NonTerminal "B"))
   )


test13 = TestCase (
   assertEqual "First set of S in grammar 3"
      (sort ["(", "x"])
      (S.toList $ symbolFirstSet grammar3 (NonTerminal "S"))
   )
test14 = TestCase (
   assertEqual "First set of A in grammar 3"
      (sort ["","+"])
      (S.toList $ symbolFirstSet grammar3 (NonTerminal "A"))
   )
test15 = TestCase (
   assertEqual "First set of T in grammar 3"
      (sort ["(","x"])
      (S.toList $ symbolFirstSet grammar3 (NonTerminal "T"))
   )
test16 = TestCase (
   assertEqual "First set of B in grammar 3"
      (sort ["","*"])
      (S.toList $ symbolFirstSet grammar3 (NonTerminal "B"))
   )
test17 = TestCase (
   assertEqual "First set of F in grammar 3"
      (sort ["(","x"])
      (S.toList $ symbolFirstSet grammar3 (NonTerminal "F"))
   )

test18 = TestCase (
   assertEqual "Follow set of S in grammar 3"
      (sort [")",""])
      (S.toList $ followSet grammar3 (NonTerminal "S"))
   )
test19 = TestCase (
   assertEqual "Follow set of A in grammar 3"
      (sort [")",""])
      (S.toList $ followSet grammar3 (NonTerminal "A"))
   )
test20 = TestCase (
   assertEqual "Follow set of T in grammar 3"
      (sort ["+",")",""])
      (S.toList $ followSet grammar3 (NonTerminal "T"))
   )
test21 = TestCase (
   assertEqual "Follow set of B in grammar 3"
      (sort ["+",")",""])
      (S.toList $ followSet grammar3 (NonTerminal "B"))
   )
test22 = TestCase (
   assertEqual "Follow set of F in grammar 3"
      (sort ["+","*",")",""])
      (S.toList $ followSet grammar3 (NonTerminal "F"))
   )


tests = TestList [
   TestLabel "First Sets" test1,
   TestLabel "First Sets" test2,
   TestLabel "First Sets" test3,
   TestLabel "First Sets" test7,
   TestLabel "First Sets" test8,
   TestLabel "First Sets" test9,
   TestLabel "First Sets" test13,
   TestLabel "First Sets" test14,
   TestLabel "First Sets" test15,
   TestLabel "First Sets" test16,
   TestLabel "First Sets" test17,
   TestLabel "Follow Sets" test4,
   TestLabel "Follow Sets" test5,
   TestLabel "Follow Sets" test6,
   TestLabel "Follow Sets" test10,
   TestLabel "Follow Sets" test11,
   TestLabel "Follow Sets" test12,
   TestLabel "Follow Sets" test18,
   TestLabel "Follow Sets" test19,
   TestLabel "Follow Sets" test20,
   TestLabel "Follow Sets" test21,
   TestLabel "Follow Sets" test22
   ]