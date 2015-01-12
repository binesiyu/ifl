module Core.CoreParse where

import           Core.CoreExpr

parse :: String -> CoreProgram
parse = syntax . clex

clex :: String -> [Token]
syntax :: [Token] -> CoreProgram
-- In Gofer I propose to compose this with some function
-- CoreProgram -> String, which will illustrate some sort of
-- execution machine, and then give this composition to catWith
-- from my utils
--
type Token = String           -- A token is never empty

isIdChar, isWhiteSpace :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')
isWhiteSpace c = c `elem` " \t\n"

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2) | (v1,toks1) <- p1 toks,
                               (v2,toks2) <- p2 toks1]
pGreeting :: Parser (String, String)
pGreeting = pThen mk_pair pHelloOrGoodbye pVar
            where
            mk_pair hg name = (hg, name)

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting

pEmpty :: a -> Parser a
pOneOrMore :: Parser a -> Parser [a]
pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting) `pApply` length

pApply :: Parser a -> (a -> b) -> Parser b
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pSat :: (String -> Bool) -> Parser String

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pNum :: Parser Int

syntax = take_first_parse . pProgram
         where
         take_first_parse ((prog,[]) : others) = prog
         take_first_parse (parse     : others) = take_first_parse others
         take_first_parse other                = error "Syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr

data PartialExpr = NoOp | FoundOp Name CoreExpr

pExpr1c :: Parser PartialExpr
pExpr1c = (pThen FoundOp (pLit "|") pExpr1) `pAlt` (pEmpty NoOp)

pExpr1 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2
