module Core where

import           Core.CoreExpr
{- import           Core.CoreParse -}
import           Core.CorePrelude
{- import           Core.CorePrint -}

bindersOf :: [(a,b)] -> [a]
bindersOf defns =  [name | (name, rhs) <- defns]

rhssOf        :: [(a,b)] -> [b]
rhssOf defns  =  [rhs  | (name, rhs) <- defns]


{- mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr -}
{- mkMultiAp n e1 e2 = foldll EAp e1 (take n e2s) -}
                    {- where -}
                    {- e2s = e2 : e2s -}

