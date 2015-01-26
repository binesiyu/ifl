module PrettyPrint where
import Language
import Utils

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline
            deriving(Show, Eq)

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
  where
    e2s = e2: e2s

iNil :: Iseq
iNil = INil
iStr :: String -> Iseq
iStr str =  IStr str

iAppend::Iseq -> Iseq -> Iseq
iAppend seq1 seq2 = IAppend seq1 seq2

iNewline::Iseq
iNewline = INewline
iIndent::Iseq -> Iseq
iIndent seq = IIndent seq


pprProgram::CoreProgram -> [Iseq]
pprProgram = concatMap (\x -> [iNewline, (iIndent . ppr) x])
  where
    ppr (n, ns, expr) = (iStr (n ++ " ")) `iAppend` (iConcat (map (\x -> iStr (x ++ " ")) ns)) `iAppend` iStr " = " `iAppend` (pprExpr expr)

pprExpr::CoreExpr -> Iseq
pprExpr (ENum n) = iNum n
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr) =
  iConcat [iStr keyword, iNewline,
          iStr " ", iIndent (pprDefns defns), iNewline,
          iStr "in ", pprExpr expr]
  where
    keyword | not isrec = "let"
            | isrec = "letrec"
pprExpr (ECase expr alts) = iConcat $
                            ([iStr "case ", pprExpr expr, iStr " of", iNewline]) ++ ( map alt alts)
  where
    alt :: (Int, [Name], Expr Name) -> Iseq
    alt (i, ns, expr) = (iConcat (map iStr ns)) `iAppend` iStr "->" `iAppend` pprExpr expr `iAppend` iNewline
    

pprExpr expr = error $ show expr
pprAExpr::CoreExpr ->  Iseq
pprAExpr e 
  | isAtomicExpr e = pprExpr e
  | otherwise  = iConcat [iStr "(", pprExpr e, iStr ")"]


pprDefns ::[(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [iStr ";", iNewline]
pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) =
  iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]
iConcat:: [Iseq] -> Iseq
iConcat = foldl iAppend iNil
iInterleave::Iseq -> [Iseq] -> Iseq
iInterleave _ [] = iNil
iInterleave _ [n] = n
iInterleave sep (n:ns) = foldl (\x y -> x `iAppend` sep `iAppend` y) n ns

iNum :: Int -> Iseq
iNum n = iStr (show n)

iFWNum:: Int -> Int -> Iseq
iFWNum width n =
  iStr (space (width - length digits) ++ digits)
  where
    digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
  where
    lay_item (n, seq) = iConcat [iFWNum 4 n, iStr ") ", iIndent seq, iNewline]

iDisplay seq = flatten 4 [(seq, 0)]

flatten::Int -> [(Iseq, Int)] -> String
flatten _ [] = ""
flatten col ((INil, k) : seqs) = flatten col seqs
flatten col ((IStr s,k) : seqs) = s ++ flatten col seqs
flatten col ((INewline, indent) : seqs) =
  '\n' : (space (col )) ++ (flatten (col) seqs)
flatten col ((IIndent seq, indent):seqs) =
  flatten (col) ((seq, (indent +col)):seqs)
 
flatten col ((IAppend seq1 seq2, k) : seqs) =
  flatten col ((seq1, k):(seq2, k):seqs)
