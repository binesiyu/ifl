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

module Core.Language where

data Expr a = EVar String            -- variables
            | ENum Int               -- numbers
            | EConstr Int Int        -- constructor tar arity
            | EAp (Expr a) (Expr a)  -- applications
            | ELet                   -- let(rec) expression
                IsRec                  -- recursive? (boolean)
                [(a, Expr a)]          -- definitions
                (Expr a)               -- body of let(rec)
            | ECase                  -- case expression
                (Expr a)               -- expression to scrutinize
                [Alter a]              -- alternatives
            | ELam [a] (Expr a)      -- lambda abstractions
            deriving (Show, Eq)
type Name     = String
type CoreExpr = Expr Name

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]

type Alter a = ( Int            -- tag
               , [a]            -- list of bound variables
               , Expr a         -- expression to the right of the arrow
               )

type CoreAlt = Alter Name

-- | Return 'True' if passed to an expression with no internal
-- structure.
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr _        = False

-- | A Core language program is just a list of supercombinator
-- defintions.
type Program a = [ScDefn a]
type CoreProgram = Program Name

-- | Supercombinator definition.
type ScDefn a = ( Name          -- name of the supercombinator
                , [a]           -- its arguments
                , Expr a        -- its body
                )
type CoreScDefn = ScDefn Name

-- | Standard prelude.
{-
I x           = x;
K x y         = x;
K1 x y        = y;
S f g x       = f x (g x);
compose f g x = f (g x);
twice f       = compose f f
-}
preludeDefs :: CoreProgram
preludeDefs =
  [ ("I", ["x"], EVar "x")
  , ("K", ["x","y"], EVar "x")
  , ("K1", ["x", "y"], EVar "y")
  , ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                             (EAp (EVar "g") (EVar "x")))
  , ("compose", ["f","g","x"], EAp (EVar "f")
                                   (EAp (EVar "g") (EVar "x")))
  , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f"))
                         (EVar "f"))
  ]
