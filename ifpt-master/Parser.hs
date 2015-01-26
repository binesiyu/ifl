module Parser where

import qualified Data.Char as C (isAlpha, isDigit, isSpace)
import qualified Data.List as L (isPrefixOf)

import Language
import Utils


type Token = String


parse::String -> CoreProgram
parse = syntax . clex


-- code for lexer
twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]


clex:: String -> [Token]
clex cs
  | L.isPrefixOf "--" cs = clex cs'
  where
    cs' = drop 1 . dropWhile (not . isNewline) $ cs
clex cs
  |( or $ map (flip L.isPrefixOf cs) twoCharOps) = (take 2 cs) : clex (drop 2 cs)


clex (c:cs)
  | isNewline c = clex cs
  | isWhiteSpace c = clex cs
  | isDigit c = (num_token) : clex rest_cs
  where
    num_token = c : takeWhile isDigit cs
    rest_cs = dropWhile isDigit cs

clex (c:cs)                
  | isAlpha c = (var_tok) : clex rest_cs
  where
    var_tok = c : takeWhile isIdChar cs
    rest_cs = dropWhile isIdChar cs

clex (c:cs) = ([c]) : clex cs
clex [] = []


isNewline c = c == '\n'
isAlpha = C.isAlpha
isDigit = C.isDigit
isWhiteSpace = C.isSpace
isIdChar c = isAlpha c || isDigit c || (c == '_')


--- parser code
keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]
type Parser a = [Token] -> [(a, [Token])]
pSat:: (String -> Bool) -> Parser String
pSat f [] = []
pSat f (tk:tks) = if f tk then
                    [(tk, tks)]
                  else
                    []

pLit :: String -> Parser String
pLit s = pSat (== s)
pVar::Parser Name
pVar [] = []
pVar tks = pSat isVar tks
  where
    isVar ss | ss == []  = error "clex error"    
    isVar ss | ss `elem` keywords = False
    isVar (a:as) = elem a (['a'..'z']++[ 'A'..'Z'])
pNum :: Parser Int
pNum  = pApply (pSat isNum) toInt
  where
    isNum  = and . map isDigit 
    isDigit = flip elem ['0'..'9']
    toInt as = (read as)::Int


pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [(combine v1 v2, toks2)| (v1, toks1) <- p1 toks, (v2, toks2) <-
        p2 toks1]
pThen3 :: (a -> b -> c -> d) -> Parser a-> Parser b-> Parser c -> Parser d
pThen3 f pa pb pc toks =[ (f a b c, tksc) | (a, tksa) <- pa toks, (b, tksb) <- pb tksa, (c, tksc) <- pc tksb]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 f pa pb pc pd toks =
  [(f a b c d, tksd)| (a, tksa) <- pa toks, (b, tksb) <- pb tksa, (c, tksc) <- pc tksb, (d, tksd) <- pd tksc]

pEmpty :: a -> Parser a
pEmpty a tks = [(a, tks)]


pAlt::Parser a -> Parser a -> Parser a
{-pAlt p1 p2 toks =if null rs1 then
                   rs2
                 else
                   rs1
  where
    rs1 = p1 toks
    rs2 = p2 toks
-}
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)


--i4 = pOneOrMore (pLit "H") ["H", "H", "H"]
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p tks = [ (a:as, tks'')| (a, tks') <- p tks, (as, tks'') <- pZeroOrMore p tks']
{-pOneOrMore p tks = pp p [] tks
  where
    pp p [] [] = []
    pp p rs [] = [(rs, [])]
    pp p rs tks = case res of
      [] -> [(rs, tks)]
      [(r', tks')] -> pp p (rs++[r']) tks'
      where
        res = p tks
-}
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty []) 

pApply::Parser a -> (a -> b) => Parser b
pApply p f tks = [(f a, tks') | (a, tks') <- p tks]

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p sep  = pThen f (pZeroOrMore p') p
  where
    p' tks = [(a, tks'')|(a, tks') <- p tks, (b, tks'') <- sep tks']
    f a b = b:a

syntax::[Token] -> CoreProgram
syntax = take_first_parse . pProgram
  where
    take_first_parse ((prog, []) : others) = prog
    take_first_parse (parse : others) = take_first_parse others
    take_first_parse other = error "Syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where
    mk_sc a b _ d  = (a, b, d)

pDefns = pOneOrMoreWithSep pDefn (pLit ";")
pDefn = pThen3 mk_def pVar (pLit "=") pExpr
  where
    mk_def a _ c = (a, c)

pAlts = pOneOrMoreWithSep pChoice (pLit ";")
pChoice = pThen4 mk_alt pN (pOneOrMore pVar) (pLit "->") pExpr
  where
    mk_alt a b c d = (a, b, d)
    pN = pThen3 take_num (pLit "<") pNum (pLit ">")
    take_num a b c = b


pExpr = pExpr1 `pAlt` pLet `pAlt` pLetrec `pAlt` pCase `pAlt` pLambda `pAlt` pAExpr 

pAExpr = pVar' `pAlt` pNum' `pAlt` ppExpr -- `pAlt` pPack
  where
    pVar' = pApply pVar EVar
    pNum' = pApply pNum ENum
    f a b c = read b::Int
--    pPack = pThen4 f (pLit "Pack") (pLit "{") pNum' (pLit "}")
    ppExpr = pThen3 g (pLit "(") pExpr (pLit ")")
    g a b c = b

pLet = pThen4 f (pLit "let") pDefns (pLit "in") pExpr
  where
    f a b c d = ELet False b d

pLetrec = pThen4 f (pLit "letrec") pDefns (pLit "in") pExpr
  where
    f a b c d = ELet True b d

pCase =  pThen4 f (pLit "case") pExpr (pLit "of") pAlts
  where
    f a b c d = ECase b d

  
pLambda = pThen4 f (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr
  where
    f a b c d = ELam b d


assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

pExpr1 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c
pExpr1c :: Parser PartialExpr
pExpr1c = (pThen FoundOp (pLit "|") pExpr1) `pAlt` (pEmpty NoOp)


pExpr2 :: Parser CoreExpr
pExpr2 = pThen assembleOp pExpr3 pExpr2c
pExpr2c::Parser PartialExpr
pExpr2c = (pThen FoundOp (pLit "&") pExpr2) `pAlt` (pEmpty NoOp)

pExpr3 :: Parser CoreExpr
pExpr3 = pThen assembleOp pExpr4 pExpr4c
pExpr3c = (pThen FoundOp pRelOp pExpr4) `pAlt` (pEmpty NoOp)

pExpr4 :: Parser CoreExpr
pExpr4 = pThen assembleOp pExpr5 pExpr4c
pExpr4c = (pThen FoundOp (pLit "+") pExpr4) `pAlt`
          (pThen FoundOp (pLit "-") pExpr5) `pAlt`
          (pEmpty NoOp)
pExpr5 :: Parser CoreExpr
pExpr5 = pThen assembleOp pExpr6 pExpr5c
pExpr5c = (pThen FoundOp (pLit "*") pExpr5) `pAlt`
          (pThen FoundOp (pLit "/") pExpr6) `pAlt`
          (pEmpty NoOp)

pExpr6 = pApply (pOneOrMore pAExpr)  mk_ap_chain
  where
    mk_ap_chain ces = foldl1 (\x y -> EAp x y) ces


pRelOp =
  (pLit "<") `pAlt` (pLit "<=")`pAlt`
  (pLit "==")`pAlt` (pLit "~=")`pAlt`
  (pLit ">=")`pAlt` (pLit ">")




