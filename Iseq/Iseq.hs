module Iseq.Iseq(Iseq(..)) where


import           Iseq.IseqRep
--import Iseq.IseqList

iNum :: Int -> Iseq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseq
iFWNum width n
  = iStr (space (width - length digits) ++ digits)
    where
    digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
             where
             lay_item (n, seq)
               = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]
