module ParserTest where
import Parser

import Data.Char

a = pLit "hello" ["hello", "John", "!"]
b = pLit "hello" ["hellx", "John", "!"]
c = pAlt (pLit "hello") (pLit "hi") ["hello", "John", "!"]
d = pThen (++) (pLit "hello") (pLit "John") ["hello", "John"]
e = pThen ((,)) (pLit "hello") (pLit "John") ["hello", "John"]
f = pThen3 ((,,)) (pLit "hello") (pLit "John") (pLit "!") ["hello", "John", "!"]

e0 = pEmpty "a" []
e1 = pEmpty "a" ["H"]

g0 = pZeroOrMore (pLit "H") []
g1 = pZeroOrMore (pLit "H") ["G"]
g2 = pZeroOrMore (pLit "H") ["H"]
g3 = pZeroOrMore (pLit "H") ["H", "H"]
g4 = pZeroOrMore (pLit "H") ["H", "H","H"]

i0 = pOneOrMore (pLit "H") []
i1 = pOneOrMore (pLit "H") ["G"]
i2 = pOneOrMore (pLit "H") ["H"]
i3 = pOneOrMore (pLit "H") ["H", "H"]
i4 = pOneOrMore (pLit "H") ["H", "H", "H"]


toL::String -> String
toL = map toLower
h0 = pApply (pOneOrMore (pLit "H")) (map toL) []
h1 = pApply (pOneOrMore (pLit "H")) (map toL) ["H"]
h2 = pApply (pOneOrMore (pLit "H")) (map toL) ["1"]
h3 = pApply (pOneOrMore (pLit "H")) (id) ["1"]
j0 = pOneOrMoreWithSep (pLit "H") (pLit ";") []
j1 = pOneOrMoreWithSep (pLit "H") (pLit ";") ["H"]
j2 = pOneOrMoreWithSep (pLit "H") (pLit ";") ["H", ";","H"]
j3 = pOneOrMoreWithSep (pLit "H") (pLit ";") ["H", ";","H", ";"]

k0 = pSat ((==) "H") []
k1 = pSat ((==) "H") ["H"]
k2 = pSat ((==) "H") ["H","H"]

k3 = pSat ((==) "H") []
l0 = pNum []
l1 = pNum ["H"]
l2 = pNum ["1"]
l3 = pNum ["12"]

m0 = syntax . clex $ "f = 3 ;"

le0 = pLet . clex $ "let z = x; in z"
le1 = pLet . clex $ "let z = x in z"
le2 = pLet . clex $ "let z = x; y = x in z"
le3 = pLetrec . clex $ "letrec z = x; y = x in z"

ae0 = pAExpr ["f"]

ca0 = pCase . clex $ "case z of <1> a b c-> z"
al0 = pChoice . clex $ "<1> a -> z"

sa0 = syntax . clex $ "f = x"

la0 = pLambda . clex $ "\\a . a"

ex0 =  pExpr . clex $ " a + b"
