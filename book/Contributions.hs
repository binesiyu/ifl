abstype iseq
with iNil :: iseq   || The empty iseq
iStr :: [char] -> iseq || Turn a string into an iseq
iAppend :: iseq -> iseq -> iseq || Append two iseqs
iNewline:: iseq   || Newline with indentation
iIndent :: iseq -> iseq  || Indent an iseq
iDisplay:: iseq -> [char] || Turn an iseq into a string

iseq == iseqRep
iseqRep ::= INil | IStr [char] | IAppend iseqRep iseqRep
                | IIndent iseqRep | INewline
iNil   = INil
iStr str  = IStr str
iAppend seq1 seq2 = IAppend seq1 seq2
iIndent seq  = IIndent seq
iNewline  = INewline
iDisplay seq  = flatten 0 [(seq,0)]

flatten :: num    || Current column; 0 for first column
        -> [(iseqRep,num)] || Work list
        -> [char]  || Result

flatten col [] = ""
flatten col ((INil,indent):seqs)
= flatten col seqs
flatten col ((IStr s,indent) : seqs)
= s ++ flatten (col+#s) seqs
flatten col ((IAppend seq1 seq2,indent) : seqs)
= flatten col ((seq1,indent) : (seq2,indent) : seqs)
flatten col ((IIndent seq, indent) : seqs)
= flatten col ((seq, col) : seqs)
flatten col ((INewline, indent) : seqs)
= "\n" ++ spaces indent ++ flatten indent seqs
flatten col [] = ""
flatten col ((seq,indent):seqs) = flatt col seq indent seqs

flatt col INil indent seqs
= flatten col seqs
flatt col (IStr s) indent seqs
= s ++ flatten (col+#s) seqs
flatt col (IAppend seq1 seq2) indent seqs
= flatten col ((seq1,indent) : (seq2,indent) : seqs)
flatt col (IIndent seq) indent seqs
= flatten col ((seq, col) : seqs)
flatt col INewline indent seqs
= "\n" ++ spaces indent ++ flatten indent seqs
flatten :: num    || Current column; 0 for first column
        -> [(iseqRep,num)] || Work list
        -> [char]  || Result

iNil col indent seqs
= flatten col seqs
iStr str col indent seqs
= s ++ flatten (col+#s) seqs
iAppend seq1 seq2 col indent seqs
= flatten col ((seq1,indent) : (seq2,indent) : seqs)
iIndent seq col indent seqs
= flatten col ((seq, col) : seqs)
iNewline col indent seqs
iDisplay seq
= flatten 0 [(seq,0)]

flatten col [] = ""
flatten col ((seq,indent):seqs) = seq col indent seqs
sNil col = ""
sCons seq indent seqs col = seq col indent seqs

iNil col indent seqs
= flatten col seqs
iStr str col indent seqs
= s ++ flatten (col+#s) seqs
iAppend seq1 seq2 col indent seqs
= flatten col (sCons seq1 indent (sCons seq2 indent seqs))
iIndent seq col indent seqs
= flatten col (sCons seq col seqs)
iNewline col indent seqs
iDisplay seq
= flatten 0 (sCons seq 0 sNil)

flatten :: num    || Current column; 0 for first column
        -> (num -> char) || Work list as function
        -> [char]  || Result

flatten col seqs = seqs col
abstype iseq
with iNil :: iseq   || The empty iseq
iStr :: [char] -> iseq || Turn a string into an iseq
iAppend :: iseq -> iseq -> iseq || Append two iseqs
iNewline:: iseq   || Newline with indentation
iIndent :: iseq -> iseq  || Indent an iseq
iDisplay:: iseq -> [char] || Turn an iseq into a string

iseq == num -> iseqs -> iseqs
iseqs == num -> [char]

sNil col = ""

iNil indent seqs  = seqs
iStr str indent seqs col = s ++ seqs (col+#s)
iAppend seq1 seq2 indent seqs = seq1 indent (seq2 indent seqs)
iIndent seq indent seqs col = seq col seqs col
iNewline indent seqs col = "\n" ++ spaces indent ++ seqs indent
iDisplay seq = seq 0 sNil 0
