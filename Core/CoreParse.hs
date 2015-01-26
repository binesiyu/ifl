module Core.CoreParse where

import           Core.CoreExpr
import           Data.Char
import           Data.Tuple

parse :: String -> CoreProgram
parse = syntax . clex
-- In Gofer I propose to compose this with some function
-- CoreProgram -> String, which will illustrate some sort of
-- execution machine, and then give this composition to catWith
-- from my utils

type Token = String           -- A token is never empty

clex :: String -> [Token]
clex (c:cs) | isWhiteSpace c = clex cs
clex (c:cs) | isDigit c = num_token : clex rest_cs
             where
             num_token = c : takeWhile isDigit cs
             rest_cs   = dropWhile isDigit cs
clex (c:cs) | isAlpha c = var_tok : clex rest_cs
             where
             var_tok = c : takeWhile isIdChar cs
             rest_cs = dropWhile isIdChar cs
clex (c:cs) = [c] : clex cs
clex [] = []

isIdChar, isWhiteSpace :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')
isWhiteSpace c = c `elem` " \t\n"

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

type Parser a = [Token] -> [(a, [Token])]


pVar :: Parser String
pVar (x:xs)     = [(x,xs)]
pVar []         = []

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [(f v1,toks1) | (v1,toks1) <- p toks ]

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2) | (v1,toks1) <- p1 toks,
                               (v2,toks2) <- p2 toks1]

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 f p1 p2 p3 toks
 = [ (f v1 v2 v3,toks3) | (v1,toks1) <- p1 toks,
                           (v2,toks2) <- p2 toks1,
                           (v3,toks3) <- p3 toks2]

pThen4 :: (a -> b -> c -> d ->f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser f
pThen4 f p1 p2 p3 p4 toks
 = [ (f v1 v2 v3 v4,toks4) | (v1,toks1) <- p1 toks,
                           (v2,toks2) <- p2 toks1,
                           (v3,toks3) <- p3 toks2,
                           (v4,toks4) <- p4 toks3]

pSat :: (String -> Bool) -> Parser String
pSat f (tok:toks) | (f tok) = [(tok, toks)]
                  | otherwise = []
pSat _ []         = []

pLit :: String -> Parser String
pLit s = pSat (== s)

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pGreeting :: Parser (String, String)
pGreeting = pThen mk_pair pHelloOrGoodbye pVar
            where
            mk_pair hg name = (hg, name)

pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting

pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting) `pApply` length

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pEmpty :: a -> Parser a
pEmpty v toks = [(v,toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p $ pZeroOrMore p


pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = undefined
{- pOneOrMoreWithSep p1 p2 toks = [(v2,toks2) | (v1,toks1) <- p1 toks, -}
                                             {- (v2,toks2) <- p2 toks1] -}


keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]


syntax :: [Token] -> CoreProgram
syntax = take_first_parse . pProgram
         where
         take_first_parse ((prog,[]) : others) = prog
         take_first_parse (parse     : others) = take_first_parse others
         take_first_parse other                = error "Syntax error"

pProgram :: Parser CoreProgram
pProgram = undefined
{- pProgram = pOneOrMoreWithSep pSc (pLit ";") -}

pSc :: Parser CoreScDefn
pSc = undefined
{- pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr -}

data PartialExpr = NoOp | FoundOp Name CoreExpr

pExpr1c :: Parser PartialExpr
pExpr1c = (pThen FoundOp (pLit "|") pExpr1) `pAlt` (pEmpty NoOp)

pExpr1 :: Parser CoreExpr
pExpr1 = undefined
{- pExpr1 = pThen assembleOp pExpr2 pExpr1c -}

assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2
