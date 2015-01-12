module Core.CoreExpr where

data Expr a
  =  EVar Name                     -- Variables
   | ENum Int                      -- Numbers
   | EConstr Int Int               -- Constructor tag arity
   | EAp (Expr a) (Expr a)         -- Applications
   | ELet                          -- Let(rec) expressions
        IsRec                      --   boolean with True = recursive,
        [(a, Expr a)]              --   Definitions
        (Expr a)                   --   Body of let(rec)
   | ECase                         -- Case expression
        (Expr a)                   --   Expression to scrutinise
        [Alter a]                  --   Alternatives
   | ELam [a] (Expr a)             -- Lambda abstractions
    deriving (Show)

type CoreExpr = Expr Name
type Name = String
type IsRec = Bool

type ScDefn a = (Name, [a], Expr a)
type Program a = [ScDefn a]
type Alter a = (Int, [a], Expr a)

type CoreScDefn = ScDefn Name
type CoreProgram = Program Name
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False
