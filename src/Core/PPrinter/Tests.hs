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

module Core.PPrinter.Tests (tests) where

import Test.HUnit (Test(..), (~=?))
import Core.Language
import Core.PPrinter

tests = TestList $
  [ TestLabel "Exercise 1.6: 'ELet' indentation" $
      "let\n x = 42\nin x" ~=?
      (ppr $ ELet nonRecursive [("x", ENum 42)] (EVar "x"))
  , TestLabel "'ELet': letrec" $
      "letrec\n x = 42\nin x" ~=?
      (ppr $ ELet recursive [("x", ENum 42)] (EVar "x"))
  , TestLabel "'ELet': two bindings" $
      "let\n x = 42;\n y = 43\nin x" ~=?
      (ppr $ ELet nonRecursive [("x", ENum 42), ("y", ENum 43)] (EVar "x"))
  , TestLabel "'ELet': nested binding" $
      "let\n x = let\n  x' = 42\n in x';\n y = 43\nin x" ~=?
      (ppr $ ELet nonRecursive
       [ ("x", (ELet nonRecursive [("x'", ENum 42)] (EVar "x'")))
       , ("y", ENum 43)
       ] (EVar "x"))
  , TestLabel "'ELet': nested expression" $
      -- XXX: shouldn't it return "let\n x = 42\nin let\n    y = 43\n   in y"?
      "let\n x = 42\nin let\n y = 43\nin y" ~=?
      (ppr $ ELet nonRecursive [("x", ENum 42)]
       (ELet nonRecursive [("y", ENum 43)] (EVar "y")))
  , TestLabel "'ECase': trivial" $
      "case 42 of\n 1 x -> x" ~=?
      (ppr $ ECase (ENum 42) [(1, ["x"], EVar "x")])
  , TestLabel "'ECase': no variables" $
      "case 42 of\n 1 -> 43" ~=?
      (ppr $ ECase (ENum 42) [(1, [], ENum 43)])
  , TestLabel "'ECase': two cases" $
      "case 42 of\n 1 x y -> y;\n 1 x -> x" ~=?
      (ppr $ ECase (ENum 42) [(1, ["x","y"], EVar "y"), (1, ["x"], EVar "x")])
  , TestLabel "'ECase': nested case" $
      "case 42 of\n 1 x y -> case y of\n  1 -> 43;\n 1 x -> x" ~=?
      (ppr $ ECase (ENum 42)
       [ (1, ["x","y"], ECase (EVar "y") [(1, [], ENum 43)])
       , (1, ["x"], EVar "x")
       ])
  , TestLabel "Precedence: parentheses" $
      "(x + y) * z" ~=?
      (ppr $ EAp (EAp (EVar "*")
                      (EAp (EAp (EVar "+")
                                (EVar "x"))
                           (EVar "y")))
                 (EVar "z"))
  , TestLabel "Precedence: no parentheses" $
      "x + y * z" ~=?
      (ppr $ EAp (EAp (EVar "+")
                      (EVar "x"))
                 (EAp (EAp (EVar "*")
                           (EVar "y"))
                      (EVar "z")))
  , TestLabel "Precedence: prefix function application" $
      "x + f y z" ~=?
      (ppr $ EAp (EAp (EVar "+")
                      (EVar "x"))
                 (EAp (EAp (EVar "f")
                           (EVar "y"))
                      (EVar "z")))
  , TestLabel "Exercise 1.8: Infix operator application" $
      "x + y > p * length xs" ~=?
      (ppr $ EAp (EAp (EVar ">")
                      (EAp (EAp (EVar "+")
                                (EVar "x"))
                           (EVar "y")))
                 (EAp (EAp (EVar "*")
                           (EVar "p"))
                      (EAp (EVar "length")
                           (EVar "xs"))))
  , TestLabel "'ELam' indentation" $
      "\\x y z .\n x" ~=?
      (ppr $ ELam ["x","y","z"] (EVar "x"))
  ]
    where
      ppr = iDisplay . flip pprExpr 0