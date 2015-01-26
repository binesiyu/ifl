{- ifl-haskell: "Implementing Functional Languages: a tutorial" in Haskell.
   Copyright 2014 Nikita Karetnikov <nikita@karetnikov.org>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Core.Parser.Tests (tests) where

import Test.HUnit (Test(..), (~=?))
import Core.Parser
import Core.Language

tests = TestList $
  [ TestLabel "Exercise 1.9: Ignore comments" $
      [(1,"foo"), (2,"bar")] ~=? (clex "foo||a comment\nbar" 1)
  , TestLabel "Exercise 1.10: Recognize two-character operators" $
      [ (1,"=="), (1,"~="), (1,"~"), (1,"~"), (1,">=")
      , (1,"<="), (1,"->"), (1,"<"), (1,"-")
      ] ~=? (clex "== ~= ~~ >= <= -> <-" 1)
  , TestLabel "Exercise 1.11: Attach a line number to each token" $
      [(1,"foo"), (3,"bar")] ~=? (clex "foo \n\n bar" 1)
  , TestLabel "'pLit': empty input" $
      (Left . ParseError 1 "empty input" $ show "foo") ~=? (pLit "foo" [])
  , TestLabel "'pLit': matching string" $
      Right ("foo", [(1,"bar")]) ~=? (pLit "foo" $ [(1,"foo"), (1,"bar")])
  , TestLabel "'pLit': failure to parse" $
      (Left . ParseError 1 (show "hello") $ show "goodbye") ~=?
        (pLit "goodbye" [(1,"hello"), (1,"rest")])
  , TestLabel "'pVar': variable" $
      Right ("foo", [(2,"bar")]) ~=? (pVar [(1,"foo"), (2,"bar")])
  , TestLabel "'pVar': not a variable" $
      (Left $ ParseError 1 (show "1") "a variable") ~=?
        (pVar [(1,"1"), (1,"foo")])
  , TestLabel "Exercise 1.17: 'pVar': ignore keywords" $
      (Left $ ParseError 1 (show "let") "a variable") ~=?
        (pVar [(1,"let"), (1,"!")])
  , TestLabel "Exercise 1.18: 'pNum': integer" $
      Right (4242, [(1,"!")]) ~=? (pNum [(1,"4242"), (1,"!")])
  , TestLabel "Exercise 1.18: 'pNum': integer" $
      (Left $ ParseError 1 "empty input" "an integer") ~=? (pNum [])
  , TestLabel "Exercise 1.18: 'pNum': parse failure" $
      (Left $ ParseError 1 (show "42.42") "an integer") ~=?
        (pNum [(1,"42.42"), (1,"!")])
  , TestLabel "'pAlt': first match" $
      Right ("hello", [(1,"rest")]) ~=?
        (pHelloOrGoodbye [(1,"hello"), (1,"rest")])
  , TestLabel "'pAlt': second match" $
      Right ("goodbye", [(1,"rest")]) ~=?
        (pHelloOrGoodbye [(1,"goodbye"), (1,"rest")])
  , TestLabel "'pAlt': parse failure" $
      (Left . ParseError 1 (show "foo") $
       (show "hello") ++ " or " ++ (show "goodbye")) ~=?
         (pHelloOrGoodbye [(1,"foo"), (1,"rest")])
  , TestLabel "'pThen': both parsers succeed" $
      Right (("hello","James"), [(1,"!")]) ~=?
        (pGreeting [(1,"hello"), (1,"James"), (1,"!")])
  , TestLabel "'pThen': first parser fails" $
      (Left . ParseError 1 (show "hi") $
       show "hello" ++ " or " ++ show "goodbye") ~=?
         (pGreeting [(1,"hi"), (1,"James"), (1,"!")])
  , TestLabel "'pThen': second parser fails" $
      (Left $ ParseError 1 (show "42") "a variable") ~=?
        (pGreeting [(1,"hello"), (1,"42"), (1,"!")])
  , TestLabel "'pThen': both parsers fail" $
      (Left . ParseError 1 (show "hi") $
       show "hello" ++ " or " ++ show "goodbye") ~=?
         (pGreeting [(1,"hi"), (1,"42"), (1,"!")])
  , TestLabel "Exercise 1.12: 'pThen3': all succeed" $
      Right (("hello","James"), []) ~=?
        (pGreeting3 [(1,"hello"), (1,"James"), (1,"!")])
  , TestLabel "'pThen3': first parser fails" $
      (Left . ParseError 1 (show "hi") $
       show "hello" ++ " or " ++ show "goodbye") ~=?
         (pGreeting3 [(1,"hi"), (1,"James"), (1,"!")])
  , TestLabel "'pThen3': second parser fails" $
      (Left $ ParseError 1 (show "42") "a variable") ~=?
        (pGreeting3 [(1,"hello"), (1,"42"), (1,"!")])
  , TestLabel "'pThen3': third parser fails" $
      (Left $ ParseError 1 (show "?") (show "!")) ~=?
        (pGreeting3 [(1,"hello"), (1,"James"), (1,"?")])
  , TestLabel "Exercise 1.12: 'pThen4': all succeed" $
      Right (("foobarbazqux"),[(1, "quux")]) ~=?
        (let combine a b c d = a ++ b ++ c ++ d
         in pThen4 combine (pLit "foo") (pLit "bar") (pLit "baz") (pLit "qux")
              [(1, "foo"), (1, "bar"), (1, "baz"), (1, "qux"), (1, "quux")])
  , TestLabel "'pThen4': first parser fails" $
      (Left $ ParseError 1 (show "notfoo") (show "foo")) ~=?
        (let combine a b c d = a ++ b ++ c ++ d
         in pThen4 combine (pLit "foo") (pLit "bar") (pLit "baz") (pLit "qux")
              [(1, "notfoo"), (1, "bar"), (1, "baz"), (1, "qux"), (1, "quux")])
  , TestLabel "'pThen4': second parser fails" $
      (Left $ ParseError 1 (show "notbar") (show "bar")) ~=?
        (let combine a b c d = a ++ b ++ c ++ d
         in pThen4 combine (pLit "foo") (pLit "bar") (pLit "baz") (pLit "qux")
              [(1, "foo"), (1, "notbar"), (1, "baz"), (1, "qux"), (1, "quux")])
  , TestLabel "'pThen4': third parser fails" $
      (Left $ ParseError 1 (show "notbaz") (show "baz")) ~=?
        (let combine a b c d = a ++ b ++ c ++ d
         in pThen4 combine (pLit "foo") (pLit "bar") (pLit "baz") (pLit "qux")
              [(1, "foo"), (1, "bar"), (1, "notbaz"), (1, "qux"), (1, "quux")])
  , TestLabel "'pThen4': fourth parser fails" $
      (Left $ ParseError 1 (show "notqux") (show "qux")) ~=?
        (let combine a b c d = a ++ b ++ c ++ d
         in pThen4 combine (pLit "foo") (pLit "bar") (pLit "baz") (pLit "qux")
              [(1, "foo"), (1, "bar"), (1, "baz"), (1, "notqux"), (1, "quux")])
  , TestLabel "Exercise 1.13: 'pEmpty'" $
      Right ("foo",[(1,"rest")]) ~=?
        (pEmpty "foo" [(1,"rest")])
  , TestLabel "Exercise 1.13: 'pZeroOrMore': zero" $
       Right ([],[(1,"foo")]) ~=? (pGreetings [(1,"foo")])
  , TestLabel "Exercise 1.13: 'pZeroOrMore': one" $
      Right ([("hello","James")], [(1,"!")]) ~=?
        (pGreetings [(1,"hello"), (1, "James"), (1,"!")])
  , TestLabel "Exercise 1.13: 'pZeroOrMore': more" $
      Right ( [("hello","James"), ("goodbye","James"), ("hello","James")]
            , [(1,"!")]
            ) ~=?
        (pGreetings [ (1,"hello"), (1, "James"), (1, "goodbye"), (1, "James")
                    , (1, "hello"), (1, "James"), (1, "!")
                    ])
  , TestLabel "Exercise 1.13: 'pOneOrMore': one" $
      Right ([("hello", "James")], [(1,"!")]) ~=?
        (pGreetings1 [(1,"hello"), (1, "James"), (1,"!")])
  , TestLabel "Exercise 1.13: 'pOneOrMore': more" $
      Right ( [("hello","James"), ("hello","James"), ("goodbye","James")]
            , [(1,"!")]
            ) ~=?
        (pGreetings1 [ (1,"hello"), (1, "James"), (1, "hello"), (1, "James")
                     , (1, "goodbye"), (1, "James"), (1, "!")
                     ])
  , TestLabel "Exercise 1.13: 'pOneOrMore': zero" $
      (Left . ParseError 1 (show "!") $
       show "hello" ++ " or " ++ show "goodbye") ~=?
         (pGreetings1 [(1,"!")])
  , TestLabel "Exercise 1.14: 'pApply': match" $
      Right (3, [(1,"!")]) ~=?
        (pGreetingsN [ (1,"hello"), (1,"James"), (1,"hello"), (1,"James")
                     , (1,"goodbye"), (1,"James"), (1,"!")
                     ])
  , TestLabel "Exercise 1.14: 'pApply': failure" $
      (Left . ParseError 1 (show "!") $
       show "hello" ++ " or " ++ show "goodbye") ~=?
        (pGreetingsN [(1,"!")])
  , TestLabel "Exercise 1.15: 'pOneOrMoreWithSep': one match" $
      Right (["a"], [(1,"!")]) ~=?
        pOneOrMoreWithSep (pLit "a") (pLit "b") [(1,"a"), (1,"!")]
  , TestLabel "Exercise 1.15: 'pOneOrMoreWithSep': separator" $
      Right (["a", "a"], [(1,"!")]) ~=?
        pOneOrMoreWithSep (pLit "a") (pLit "b")
          [(1,"a"), (1,"b"), (1,"a"), (1,"!")]
  , TestLabel "Exercise 1.15: 'pOneOrMoreWithSep': no match" $
      (Left $ ParseError 1 (show "!") (show "a")) ~=?
        pOneOrMoreWithSep (pLit "a") (pLit "b") [(1,"!")]
  , TestLabel "Exercise 1.15: 'pOneOrMoreWithSep': no separator" $
      Right (["a"], [(1,"a"),(1,"!")]) ~=?
        pOneOrMoreWithSep (pLit "a") (pLit "b") [(1, "a"), (1,"a"), (1,"!")]
  , TestLabel "Exercise 1.21: 'parse'" $
      [ ("f", [], ENum 3)
      , ("g", ["x","y"], ELet False [("z", EVar "x")] (EVar "z"))
      , ("h", ["x"], ECase (ELet False [("y", EVar "x")] (EVar "y"))
                       [ (1, [], ENum 2)
                       , (2, [], ENum 5)
                       ])
      ] ~=?
      (parse $ "f = 3 ;\n"
            ++ "g x y = let z = x in z ;\n"
            ++ "h x = case (let y = x in y) of\n"
            ++ "        <1> -> 2 ;\n"
            ++ "        <2> -> 5")
  , TestLabel "Exercise 1.22: dangling else" $
      [("f", ["x","y"], ECase (EVar "x")
                          [(1, [], ECase (EVar "y")
                                     [ (1, [], ENum 1)
                                     , (2, [], ENum 2)
                                     ])])] ~=?
      (parse $ "f x y = case x of\n"
            ++ "        <1> -> case y of\n"
            ++ "               <1> -> 1;\n"
            ++ "        <2> -> 2")
  , TestLabel "Exercise 1.23: application" $
      [ ("foo", ["f","x","y","z"], EAp (EAp (EAp (EVar "f")
                                                 (EVar "x"))
                                            (EVar "y"))
                                       (EVar "z"))
      , ("bar", ["f","g","x","y","z"], EAp (EAp (EVar "f")
                                                (EAp (EAp (EVar "g")
                                                          (EVar "x"))
                                                     (EVar "y")))
                                           (EVar "z"))
      , ("baz", ["f","g","x","y","z"], EAp (EAp (EVar "f")
                                                (EVar "x"))
                                           (EAp (EAp (EVar "g")
                                                     (EVar "y"))
                                                (EVar "z")))
      ] ~=?
      (parse $ "foo f x y z   = f x y z;\n"
            ++ "bar f g x y z = f (g x y) z;\n"
            ++ "baz f g x y z = f x (g y z)")
  , TestLabel "Exercise 1.24: infix operators" $
      [("foo", ["x","y","p","xs"], EAp (EAp (EVar ">")
                                            (EAp (EAp (EVar "+")
                                                      (EVar "x"))
                                                 (EVar "y")))
                                       (EAp (EAp (EVar "*")
                                                 (EVar "p"))
                                            (EAp (EVar "length")
                                                 (EVar "xs"))))
      ] ~=?
      (parse "foo x y p xs = x + y > p * length xs")
  ]

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pGreeting :: Parser (String, String)
pGreeting = pThen (,) pHelloOrGoodbye pVar

pGreeting3 :: Parser (String, String)
pGreeting3 = pThen3 combine pHelloOrGoodbye pVar (pLit "!")
  where
    combine hg name exclamation = (hg, name)

pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting

pGreetings1 :: Parser [(String, String)]
pGreetings1 = pOneOrMore pGreeting

pGreetingsN :: Parser Int
pGreetingsN = pGreetings1 `pApply` length