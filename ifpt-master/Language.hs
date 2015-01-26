module Language where
import Utils

type Name = String
type IsRec = Bool
type CoreExpr = Expr Name
recursive, nonRecursive::IsRec
recursive = True
nonRecursive = False

data Expr a = EVar Name
              | ENum Int
              | EConstr Int Int
              | EAp (Expr a) (Expr a)
              | ELet IsRec [(a, Expr a)] (Expr a)
              | ECase (Expr a) [Alter a]
              | ELam [a] (Expr a)
                deriving(Show, Eq)
                
data PartialExpr = NoOp | FoundOp Name CoreExpr
                 deriving(Show, Eq)

binderOf :: [(a,b)] ->[a]
binderOf defns = [name | (name, rhs) <- defns]
rhssOf ::[(a,b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]
type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e = False
type Program a = [ScDefn a]
type CoreProgram = Program Name
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name



