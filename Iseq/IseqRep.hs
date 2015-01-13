module Iseq.IseqRep where

import           GHC.Base

data Iseq = INil
             | IStr [Char]
             | IAppend Iseq Iseq
             | IIndent Iseq
             | INewline

iNil   = INil
iStr str  = IStr str
iAppend seq1 seq2 = IAppend seq1 seq2
iIndent seq  = IIndent seq
iNewline  = INewline
iDisplay seq  = flatten 0 [(seq,0)]

spaces num = take num (repeat ' ')

flatten :: Int-- Current column; 0 for first column
        -> [(Iseq,Int)] -- Work list
        -> [Char]  -- Result

flatten col [] = ""
flatten col ((INil,indent):seqs) = flatten col seqs
flatten col ((IStr s,indent) : seqs) = s ++ flatten (col+(length s)) seqs
flatten col ((IAppend seq1 seq2,indent) : seqs) = flatten col ((seq1,indent) : (seq2,indent) : seqs)
flatten col ((IIndent seq, indent) : seqs) = flatten col ((seq, col) : seqs)
flatten col ((INewline, indent) : seqs) = "\n" ++ spaces indent ++ flatten indent seqs
flatten col [] = ""
flatten col ((seq,indent):seqs) = flatt col seq indent seqs

flatt col INil indent seqs = flatten col seqs
flatt col (IStr s) indent seqs = s ++ flatten (col+(length s)) seqs
flatt col (IAppend seq1 seq2) indent seqs = flatten col ((seq1,indent) : (seq2,indent) : seqs)
flatt col (IIndent seq) indent seqs = flatten col ((seq, col) : seqs)
flatt col INewline indent seqs = "\n" ++ spaces indent ++ flatten indent seqs

