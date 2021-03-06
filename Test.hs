import HParser.Grammar
import HParser.Generator
import HParser.FirstSet
import HParser.FollowSet

import qualified Data.Set as S
import Data.List
import Test.HUnit

import Grammar1
import Grammar2
import Grammar3
import Grammar4
import Grammar5

-- NOTE: You must install HUnit on your system to run the tests

main = runTestTT tests

-- The first and follow set tests are done with http://mdaines.github.io/grammophone/

firstSetS1 = TestCase (
   assertEqual "First set of S in grammar 1"
      (sort ["ONE", "OPEN"])
      (S.toList $ symbolFirstSet grammar1 (NonTerminal "S"))
   )
firstSetSp1 = TestCase (
   assertEqual "First set of Sp in grammar 1"
      (sort ["PLUS",""])
      (S.toList $ symbolFirstSet grammar1 (NonTerminal "Sp"))
   )
firstSetE1 = TestCase (
   assertEqual "First set of E in grammar 1"
      (sort ["ONE","OPEN"])
      (S.toList $ symbolFirstSet grammar1 (NonTerminal "E"))
   )

firstSetS2 = TestCase (
   assertEqual "First set of S in grammar 2"
      (sort ["a", "b"])
      (S.toList $ symbolFirstSet grammar2 (NonTerminal "S"))
   )
firstSetA2 = TestCase (
   assertEqual "First set of A in grammar 2"
      (sort [""])
      (S.toList $ symbolFirstSet grammar2 (NonTerminal "A"))
   )
firstSetB2 = TestCase (
   assertEqual "First set of B in grammar 2"
      (sort [""])
      (S.toList $ symbolFirstSet grammar2 (NonTerminal "B"))
   )

firstSetS3 = TestCase (
   assertEqual "First set of S in grammar 3"
      (sort ["OPEN", "x"])
      (S.toList $ symbolFirstSet grammar3 (NonTerminal "S"))
   )
firstSetA3 = TestCase (
   assertEqual "First set of A in grammar 3"
      (sort ["","PLUS"])
      (S.toList $ symbolFirstSet grammar3 (NonTerminal "A"))
   )
firstSetT3 = TestCase (
   assertEqual "First set of T in grammar 3"
      (sort ["OPEN","x"])
      (S.toList $ symbolFirstSet grammar3 (NonTerminal "T"))
   )
firstSetB3 = TestCase (
   assertEqual "First set of B in grammar 3"
      (sort ["","PRODUCT"])
      (S.toList $ symbolFirstSet grammar3 (NonTerminal "B"))
   )
firstSetF3 = TestCase (
   assertEqual "First set of F in grammar 3"
      (sort ["OPEN","x"])
      (S.toList $ symbolFirstSet grammar3 (NonTerminal "F"))
   )

followSetS1 = TestCase (
   assertEqual "Follow set of S in grammar 1"
      (sort ["CLOSE", ""])
      (S.toList $ followSet grammar1 (NonTerminal "S"))
   )
followSetSp1 = TestCase (
   assertEqual "Follow set of Sp in grammar 1"
      (sort ["CLOSE",""])
      (S.toList $ followSet grammar1 (NonTerminal "Sp"))
   )
followSetE1 = TestCase (
   assertEqual "Follow set of E in grammar 1"
      (sort ["PLUS","CLOSE",""])
      (S.toList $ followSet grammar1 (NonTerminal "E"))
   )

followSetS2 = TestCase (
   assertEqual "Follow set of S in grammar 2"
      (sort [""])
      (S.toList $ followSet grammar2 (NonTerminal "S"))
   )
followSetA2 = TestCase (
   assertEqual "Follow set of A in grammar 2"
      (sort ["a","b"])
      (S.toList $ followSet grammar2 (NonTerminal "A"))
   )
followSetB2 = TestCase (
   assertEqual "Follow set of B in grammar 2"
      (sort ["a","b"])
      (S.toList $ followSet grammar2 (NonTerminal "B"))
   )

followSetS3 = TestCase (
   assertEqual "Follow set of S in grammar 3"
      (sort ["CLOSE",""])
      (S.toList $ followSet grammar3 (NonTerminal "S"))
   )
followSetA3 = TestCase (
   assertEqual "Follow set of A in grammar 3"
      (sort ["CLOSE",""])
      (S.toList $ followSet grammar3 (NonTerminal "A"))
   )
followSetT3 = TestCase (
   assertEqual "Follow set of T in grammar 3"
      (sort ["PLUS","CLOSE",""])
      (S.toList $ followSet grammar3 (NonTerminal "T"))
   )
followSetB3 = TestCase (
   assertEqual "Follow set of B in grammar 3"
      (sort ["PLUS","CLOSE",""])
      (S.toList $ followSet grammar3 (NonTerminal "B"))
   )
followSetF3 = TestCase (
   assertEqual "Follow set of F in grammar 3"
      (sort ["PLUS","PRODUCT","CLOSE",""])
      (S.toList $ followSet grammar3 (NonTerminal "F"))
   )

generateGrammar1 = TestCase (
   assertBool "Generating a parser from grammar 1" $
   (genParser grammar1 "Grammar1Parser" /= "")
   )

generateGrammar2 = TestCase (
   assertBool "Generating a parser from grammar 2" $
   (genParser grammar2 "Grammar2Parser" /= "")
   )

generateGrammar3 = TestCase (
   assertBool "Generating a parser from grammar 3" $
   (genParser grammar3 "Grammar3Parser" /= "")
   )

tests = TestList [
   TestLabel "First Sets" firstSetS1,
   TestLabel "First Sets" firstSetSp1,
   TestLabel "First Sets" firstSetE1,
   TestLabel "First Sets" firstSetS2,
   TestLabel "First Sets" firstSetA2,
   TestLabel "First Sets" firstSetB2,
   TestLabel "First Sets" firstSetS3,
   TestLabel "First Sets" firstSetA3,
   TestLabel "First Sets" firstSetT3,
   TestLabel "First Sets" firstSetB3,
   TestLabel "First Sets" firstSetF3,
   TestLabel "Follow Sets" followSetS1,
   TestLabel "Follow Sets" followSetSp1,
   TestLabel "Follow Sets" followSetE1,
   TestLabel "Follow Sets" followSetS2,
   TestLabel "Follow Sets" followSetA2,
   TestLabel "Follow Sets" followSetB2,
   TestLabel "Follow Sets" followSetS3,
   TestLabel "Follow Sets" followSetA3,
   TestLabel "Follow Sets" followSetT3,
   TestLabel "Follow Sets" followSetB3,
   TestLabel "Follow Sets" followSetF3,
   TestLabel "Generating parser" generateGrammar1,
   TestLabel "Generating parser" generateGrammar2,
   TestLabel "Generating parser" generateGrammar3
   ]