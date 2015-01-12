module Iseq.Iseq where

Iseq ::= INil | IStr [char] | IAppend Iseq Iseq
                | IIndent Iseq | INewline
iNil   = INil
iStr str  = IStr str
iAppend seq1 seq2 = IAppend seq1 seq2
iIndent seq  = IIndent seq
iNewline  = INewline
iDisplay seq  = flatten 0 [(seq,0)]


flatten :: num    -- Current column; 0 for first column
        -> [(Iseq,num)] -- Work list
        -> [char]  -- Result

flatten col [] = ""
flatten col ((INil,indent):seqs) = flatten col seqs
flatten col ((IStr s,indent) : seqs) = s ++ flatten (col+#s) seqs
flatten col ((IAppend seq1 seq2,indent) : seqs) = flatten col ((seq1,indent) : (seq2,indent) : seqs)
flatten col ((IIndent seq, indent) : seqs) = flatten col ((seq, col) : seqs)
flatten col ((INewline, indent) : seqs) = "\n" ++ spaces indent ++ flatten indent seqs
flatten col [] = "" flatten col ((seq,indent):seqs) = flatt col seq indent seqs

flatt col INil indent seqs = flatten col seqs
flatt col (IStr s) indent seqs = s ++ flatten (col+#s) seqs
flatt col (IAppend seq1 seq2) indent seqs = flatten col ((seq1,indent) : (seq2,indent) : seqs)
flatt col (IIndent seq) indent seqs = flatten col ((seq, col) : seqs)
flatt col INewline indent seqs = "\n" ++ spaces indent ++ flatten indent seqs

