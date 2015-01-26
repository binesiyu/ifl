{- ifl-haskell: "Implementing Functional Languages: a tutorial" in Haskell.
   Copyright 2014 Nikita Karetnikov <nikita@karetnikov.org>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Core.Parser where

import Core.Language
import Data.Char (isDigit, isAlpha, isNumber)

type LineNumber = Int
type Token = (LineNumber, String)  -- 'Token' is never empty

clex :: String -> LineNumber -> [Token]
clex [] _ = []
clex ('|':'|':cs) n
  = clex (dropChar '\n' $ dropWhile (/='\n') cs) (n + 1)  -- ignore comments
    where
      dropChar c []     = []
      dropChar c (x:xs) = if c == x then xs else (x:xs)
clex (c1:c2:cs) n
  | isOperator [c1,c2] = (n, [c1,c2]) : clex cs n
    where
      twoCharOps   = ["==", "~=", ">=", "<=", "->"]
      isOperator o = o `elem` twoCharOps
clex (c:cs) n
  | isNewline c = clex cs (n + 1)
  | isSpace c   = clex cs n
  | isDigit c = let numToken = c : takeWhile isDigit cs
                    rest     = dropWhile isDigit cs
                in (n, numToken) : clex rest n
  | isAlpha c = let isIdChar c = isAlpha c || isDigit c || (c == '_')
                    varToken   = c : takeWhile isIdChar cs
                    rest       = dropWhile isIdChar cs
                in (n, varToken) : clex rest n
  | otherwise = (n, [c]) : clex cs n
    where
      isNewline c = c == '\n'
      isSpace c   = c `elem` " \t"

syntax :: [Token] -> CoreProgram
syntax = takeScs . pProgram
  where
    takeScs xs = case xs of
      -- The list of tokens contains only the list of
      -- supercombinators, which is what we want.
      Right (scs,[]) -> scs
      -- Something is left out, which indicates a syntax error.  We
      -- inspect those tokens to print a more informative error
      -- message.
      Right (_,ts)   -> takeScs $
        let Left e = pThen (flip const) (pLit ";") pProgram ts
        in if unexpected e == "empty input"
           -- XXX: In the "empty input" cases of 'pLit', 'pVar', and
           -- 'pNum', 'errorLine' is hardcoded to '1' since the empty
           -- list does not contain anything, including the line
           -- numbers.  There, it means that the parser is passed to
           -- the empty input (the start of a file).  But here, it
           -- means that the parser has reached the end of input (the
           -- end of a file), so the line number may be different.
           then Left $ ParseError (fst $ last ts) (unexpected e) (expected e)
           else Left e
      Left e         -> error $ "syntax error: " ++ show e

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where
    mkSc name args _ expr = (name, args, expr)

pExpr :: Parser CoreExpr
pExpr =
  pELet `pAlt` pELetrec `pAlt` pECase `pAlt` pELam `pAlt` pExpr1 `pAlt` pAExpr

data PartialExpr = NoOp
                 | FoundOp Name CoreExpr

pExpr1c :: Parser PartialExpr
pExpr1c = (pThen FoundOp (pLit "|") pExpr1) `pAlt` (pEmpty NoOp)

pExpr1 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c

pExpr2c :: Parser PartialExpr
pExpr2c = (pThen FoundOp (pLit "&") pExpr2) `pAlt` (pEmpty NoOp)

pExpr2 :: Parser CoreExpr
pExpr2 = pThen assembleOp pExpr3 pExpr2c

pExpr3c :: Parser PartialExpr
pExpr3c = (pThen FoundOp pRelop pExpr4) `pAlt` (pEmpty NoOp)
  where pRelop = (pLit "<") `pAlt` (pLit "<=") `pAlt` (pLit "==") `pAlt`
                 (pLit "~=") `pAlt` (pLit ">=") `pAlt` (pLit ">")

pExpr3 :: Parser CoreExpr
pExpr3 = pThen assembleOp pExpr4 pExpr3c

pExpr4c :: Parser PartialExpr
pExpr4c = (pThen FoundOp (pLit "+") pExpr4) `pAlt`
          (pThen FoundOp (pLit "-") pExpr5) `pAlt`
          (pEmpty NoOp)

pExpr4 :: Parser CoreExpr
pExpr4 = pThen assembleOp pExpr5 pExpr4c

pExpr5c :: Parser PartialExpr
pExpr5c = (pThen FoundOp (pLit "*") pExpr5) `pAlt`
          (pThen FoundOp (pLit "/") pExpr6) `pAlt`
          (pEmpty NoOp)

pExpr5 :: Parser CoreExpr
pExpr5 = pThen assembleOp pExpr6 pExpr5c

pExpr6 :: Parser CoreExpr
pExpr6 = pApply (pOneOrMore pAExpr) (foldl1 EAp)

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

pDefns = pOneOrMore pDefn
  where
    pDefn = pThen3 (\v _ e -> (v,e)) pVar (pLit "=") pExpr

pELet    = pThen4 mkELet (pLit "let") pDefns (pLit "in") pExpr
  where
    mkELet _ ds _ e = ELet nonRecursive ds e

pELetrec = pThen4 mkELetrec (pLit "letrec") pDefns (pLit "in") pExpr
  where
    mkELetrec _ ds _ e = ELet recursive ds e

pECase   = pThen4 mkECase (pLit "case") pExpr (pLit "of") pAlts
  where
    mkECase _ e _ as = ECase e as
    pAlts = pOneOrMoreWithSep pAlt (pLit ";")
    pAlt  = pThen4 mkAlt (pThen3 (\_ n _ -> n) (pLit "<") pNum (pLit ">"))
                         (pZeroOrMore pVar)
                         (pLit "->")
                         pExpr
    mkAlt n vs _ e = (n,vs,e)

pELam    = pThen4 mkELam (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr
  where
    mkELam _ vs _ e = ELam vs e

pAExpr   = pEVar `pAlt` pENum `pAlt` pEConstr `pAlt` pPExpr

pEVar    = pApply pVar EVar

pENum    = pApply pNum ENum

pEConstr = pThen EConstr pNum pNum

pPExpr   = pThen3 (\_ e _ -> e) (pLit "(") pExpr (pLit ")")

parse :: String -> CoreProgram
parse = syntax . flip clex 1

data ParseError = ParseError { errorLine  :: Int
                             , unexpected :: String
                             , expected   :: String
                             } deriving Eq

instance Show ParseError where
  show e = "line " ++ show (errorLine e) ++ ":" ++
           " unexpected " ++ unexpected e ++
           "; expected " ++ expected e

type Parser a = [Token] -> (Either ParseError (a, [Token]))

pLit :: String -> Parser String
pLit x []     = Left $ ParseError 1 "empty input" (show x)
pLit x ((n,s):ts)
  | x == s    = Right (s,ts)
  | otherwise = Left $ ParseError n (show s) (show x)

pVar :: Parser String
pVar []                  = Left $ ParseError 1 "empty input" "a variable"
pVar ((n,s):ts)
  | any (== s) keywords  = Left $ ParseError n (show s) "a variable"
  -- If the first character is a letter, the token is a variable (see
  -- the 'isAlpha c' case in the definition of 'clex').
  -- Since 'Token' is never empty, it is safe to use 'head' here.
  | isAlpha $ head s     = Right (s,ts)
  | otherwise            = Left $ ParseError n (show s) "a variable"
    where
      keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pNum :: Parser Int
pNum []            = Left $ ParseError 1 "empty input" "an integer"
pNum ((n,s):ts)
  | all isNumber s = Right (read s, ts)
  | otherwise      = Left $ ParseError n (show s) "an integer"

-- | Apply two parsers to the same input.
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 ts = case p1 ts of
  Right res1 -> Right res1
  Left e1    -> case p2 ts of
    Right res2 -> Right res2
    Left e2    -> Left . ParseError (errorLine e1) (unexpected e1) $
                    (expected e1) ++ " or " ++ (expected e2)

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 ts =
  case p1 ts of
    Left e1 -> Left e1
    Right (v1, ts1) -> case p2 ts1 of
      Left e2 -> Left e2
      Right (v2, ts2) -> Right (combine v1 v2, ts2)

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 ts =
  case p1 ts of
    Left e1 -> Left e1
    Right (v1, ts1) -> case p2 ts1 of
      Left e2 -> Left e2
      Right (v2, ts2) -> case p3 ts2 of
        Left e3 -> Left e3
        Right (v3, ts3) -> Right (combine v1 v2 v3, ts3)

pThen4 :: (a -> b -> c -> d -> e)
       -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 ts =
  case p1 ts of
    Left e1 -> Left e1
    Right (v1, ts1) -> case p2 ts1 of
      Left e2 -> Left e2
      Right (v2, ts2) -> case p3 ts2 of
        Left e3 -> Left e3
        Right (v3, ts3) -> case p4 ts3 of
          Left e4 -> Left e4
          Right (v4, ts4) -> Right (combine v1 v2 v3 v4, ts4)

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pEmpty :: a -> Parser a
pEmpty x ts = Right (x,ts)

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p $ pZeroOrMore p

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f ts = case p ts of
  Left e -> Left e
  Right (v, ts) -> Right (f v, ts)

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen (:) p1 (pZeroOrMore $ pThen (flip const) p2 p1)
