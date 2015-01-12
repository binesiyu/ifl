module Iseq.IseqList where

iNil :: Iseq   -- The empty Iseq
iStr :: [char] -> Iseq -- Turn a string into an Iseq
iAppend :: Iseq -> Iseq -> Iseq -- Append two Iseqs
iNewline:: Iseq   -- Newline with indentation
iIndent :: Iseq -> Iseq  -- Indent an Iseq
iDisplay:: Iseq -> [char] -- Turn an Iseq into a string

sNil col = ""

iNil indent seqs  = seqs
iStr str indent seqs col = s ++ seqs (col+#s)
iAppend seq1 seq2 indent seqs = seq1 indent (seq2 indent seqs)
iIndent seq indent seqs col = seq col seqs col
iNewline indent seqs col = "\n" ++ spaces indent ++ seqs indent
iDisplay seq = seq 0 sNil 0
