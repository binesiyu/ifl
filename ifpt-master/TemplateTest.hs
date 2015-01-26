module TemplateTest where

import PreludeDefs
import Language
import Parser
import PrettyPrint
import Template
import Utils


sc0 = pr "f x y = g x y; g y x = x;  main = f 1 2"
sc1 = pr  "g x y = let a = x in x; main = g 1 2"
sc2 = pr  "f x y = g x y;g x y = let a = x in x; main = f 1 2"
sc3 = pr  "g x y = let a = x; b = a in b; main = g 1 2"
t0 = pr "pair x y f = f x y ; fst p = p K; snd p = p K1 ; f x y = letrec a = pair 11 22 in fst a; main = f 100 200"
t1 = pr "pair x y f = f x y ; fst p = p K; snd p = p K1 ; f x y = letrec a = pair x b ; b = pair y a in fst (snd (snd (snd a))); main = f 3 4"
r1 = pr "main = letrec f = f x in f"

lt0 = pr "main = let x = 1 in x"
lt1 = pr "main = letrec x =1 ; y = x in y"
lt2 = pr "main = letrec x =y ; y = 1 in x"
lt3 = pr "main =f 3; f x = let y = x in y"




