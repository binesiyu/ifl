module Iseq.Iseq(module Iseq.IseqRep,
                iConcat,
                iInterleave,
                iNum) where


import           Iseq.IseqRep
--import Iseq.IseqList

iNum :: Int -> Iseq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseq
iFWNum width n
  = iStr (spaces (width - length digits) ++ digits)
    where
    digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
             where
             lay_item (n, seq)
               = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]

iConcat     :: [Iseq] -> Iseq
iConcat [] = iNil
iConcat (x:xs) = IAppend x (iConcat xs)

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ [] = iNil
iInterleave seq (x:xs) = IAppend (IAppend x seq) (iInterleave seq xs)
