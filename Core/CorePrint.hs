module Core.CorePrint(pprint) where

import           Core.CoreExpr
import           Iseq.Iseq

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

pprProgram :: CoreProgram -> Iseq
pprProgram pro = iInterleave seq (map pprLin pro)
                 where
                       seq = iConcat [ iStr ";", iNewline ]
                       pprLin :: ScDefn Name -> Iseq
                       pprLin (name,var,exp)
                         = iConcat [iStr name,
                                    iStr " ",
                                    iConcat (map (\a -> iAppend (iStr a) (iStr " ")) var),
                                    iStr "= ",
                                    pprExpr exp ]
pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v) = iStr v
pprExpr (ENum n) = iNum n
pprExpr (EConstr tag arity)
  = iConcat [ iStr "Pack{",
              iNum tag,
              iNum arity,
              iStr "}"]
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprExpr e2)
pprExpr (ELet isrec defns expr)
  = iConcat [  iStr keyword, iNewline,
               iStr "  ",iIndent (pprDefns defns),iNewline,
               iStr "in ",pprExpr expr ]
    where
    keyword | not isrec = "let"
            | isrec = "letrec"

    pprDefns :: [(Name,CoreExpr)] -> Iseq
    pprDefns defns = iInterleave sep (map pprDefn defns)
                     where
                     sep = iConcat [ iStr ";", iNewline ]
    pprDefn :: (Name, CoreExpr) -> Iseq
    pprDefn (name, expr)
      = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

pprExpr (ELam args exp)
  = iConcat [ iStr "\\ ",
              iConcat (map (\a -> iAppend (iStr a) (iStr " ")) args),
              iStr " ",
              pprExpr exp ]

pprExpr (ECase exp alter)
  = iConcat [ iStr "case ",
              pprExpr exp,
              iStr " of",iNewline,
              iIndent (pprAlt alter)]
    where
        pprAlt :: [Alter Name] -> Iseq
        pprAlt alter = iInterleave seq (map pprAltOne alter)
                       where seq = iConcat [iStr ";", iNewline]
        pprAltOne :: Alter Name -> Iseq
        pprAltOne (alt,val,exp)
          = iConcat [ iStr "<",
                      iNum alt,
                      iStr ">",
                      iConcat (map (\a -> iAppend (iStr a) (iStr " ")) val),
                      pprExpr exp ]
