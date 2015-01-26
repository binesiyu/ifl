module Template where

import Data.List (mapAccumL, mapAccumR, delete)

import PreludeDefs
import Language
import Parser
import PrettyPrint
import Utils

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]
type TiDump = [TiStack]
initialTiDump = []
type TiHeap = Heap Node
data Node = NAp Addr Addr
          | NSupercomb Name [Name] CoreExpr
          | NNum Int
          | NInd Addr
          | NPrim Name Primitive
            deriving(Show, Eq)
type TiGlobals = ASSOC Name Addr
data Primitive = Neg | Add | Sub | Mul | Div
               deriving(Show, Eq)
pr = printResults
printResults :: String -> IO ()
printResults = putStr . run
run::String -> String
run = showResults . eval . compile . parse


compile::CoreProgram -> TiState
compile program
 = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
   where
     sc_defs = program ++ preludeDefs ++ extraPreludeDefs
     (initial_heap, globals) = buildInitialHeap sc_defs
     initial_stack = [address_of_main]
     address_of_main = aLookup globals "main" (error "main is not defined")
     buildInitialHeap ::[CoreScDefn] -> (TiHeap, TiGlobals)
     buildInitialHeap sc_defs = (heap2, sc_addrs ++ prim_addrs)
       where
         (heap1, sc_addrs)   = mapAccumL allocateSc hInitial sc_defs
         (heap2, prim_addrs) = mapAccumL allocatePrim heap1 primitives
primitives :: ASSOC Name Primitive
primitives = [("negate", Neg), ("+", Add), ("-", Sub), ("*", Mul), ("/", Div)]
allocatePrim::TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim) = (heap', (name, addr))
  where
    (heap', addr) = hAlloc heap(NPrim name prim)

primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state (div)

primNeg::TiState -> TiState
primNeg (stack, dump, heap, globals, stats) = undefined


primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith = undefined

         


allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) =(heap', (name, addr))
  where
    (heap', addr) = hAlloc heap (NSupercomb name args body)
    

extraPreludeDefs = []

eval::TiState -> [TiState]
eval state =  state : rest_states
  where
    rest_states | tiFinal state = []
                | otherwise = eval next_state
    next_state = doAdmin (step state)
doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state



type TiStats = Int
tiStatInitial::TiStats
tiStatInitial = 0
tiStatIncSteps::TiStats -> TiStats
tiStatIncSteps s = s + 1
tiStatGetSteps::TiStats -> Int
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats) =
  (stack, dump, heap, sc_defs, stats_fun stats)

tiFinal::TiState -> Bool
tiFinal ([sole_addr], dump, heap, globals, stats) =
  isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False
isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node = False

step :: TiState -> TiState
step state = dispatch (hLookup heap (hd stack))
  where
    (stack, dump, heap, globals, stats) = state
    dispatch (NNum n) = numStep state n
    dispatch (NAp a1 a2) = apStep state a1 a2 
    dispatch (NSupercomb sc args body) = scStep state sc args body
    dispatch (NInd a) = indStep state (hd stack) a

numStep :: TiState -> Int -> TiState
numStep (a:[], stk:dump, heap, globals, stats) n = (stk, dump, heap, globals, stats)
numStep _ _ = error "invalid numStep"
  

{-- (Rule 2.1)
apStep (stack, dump, heap, globals, stats) a1 a2 = 
  (a1:stack, dump, heap, globals, stats)
--}
-- (Rule 2.8)
apStep (a:stack, dump, heap, globals, stats) a1 a2 =
  case node of
    NInd a3 -> (a:stack, dump, hUpdate heap a (NAp a1 a3), globals, stats)
    otherwise -> (a1:stack, dump, heap, globals, stats)
  where
    node = hLookup heap a2



scStep = scStep2_2
-- Rule 2.2 
scStep2_2 :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep2_2 (stack, dump, heap, globals, stats) sc_name arg_names body = (new_stack, dump, new_heap, globals, stats)
  where
    new_stack = result_addr : (drop (length arg_names +1) stack)
    (new_heap, result_addr) = instantiate body heap env
    env = arg_bindings ++ globals
    arg_bindings = zip2 arg_names (getargs heap stack)
-- Rule 2.3
scStep2_3 :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep2_3 (stack, dump, heap, globals, stats) sc_name arg_names body = (new_stack, dump, new_heap, globals, stats)
  where
    new_stack = drop (length arg_names) stack
    (heap', result_addr) = instantiate body heap env
    env = arg_bindings ++ globals
    arg_bindings = zip2 arg_names (getargs heap stack)
    new_heap = hUpdate heap' (head new_stack) (NInd result_addr)
-- Rule 2.4
indStep :: TiState -> Addr -> Addr-> TiState
indStep (top:stack, dump, heap, globals, stats) a aind = (stack', dump,heap, globals, stats)
  where
    stack' = aind:stack


s0 = pr "id x = x; main = twice twice id 3"


getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack) =   map get_arg stack
  where
    get_arg addr = arg
      where (NAp fun arg) = hLookup heap addr

instantiate :: CoreExpr -> TiHeap -> ASSOC Name Addr -> (TiHeap, Addr)
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap  env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env = (heap, aLookup env v (error$  "Undeinfed name " ++ show v))
instantiate (EConstr tag arity) heap env = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env = instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = error "Can't instantiate case exprs"
instantiateConstr tag arity heap env = error "Can't instantiate constructors yet"


instantiateLet True defs body heap env = instantiate body heap' env'
  where
    ((heap', env'), addrs') = mapAccumR f (heap, env) defs
    f :: (TiHeap, ASSOC Name Addr) -> (Name, Expr Name) -> ((TiHeap, ASSOC Name Addr), Addr)
    f (heap, env) (n, e) = ((heap1, (n,a1):env), a1)
      where
      (heap1, a1) = instantiate e heap env'
instantiateLet False defs body heap env = instantiate body heap' env'
  where
    ((heap', env'), addrs') = mapAccumR f (heap, env) defs
    f :: (TiHeap, ASSOC Name Addr) -> (Name, Expr Name) -> ((TiHeap, ASSOC Name Addr), Addr)
    f (heap, env) (n, e) = ((heap1, (n,a1):env), a1)
      where
      (heap1, a1) = instantiate e heap env


---- Mark 3
instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> ASSOC Name Addr -> TiHeap
instantiateAndUpdate (ENum n) upd_addr heap env = undefined
instantiateAndUpdate (EVar v) upd_addr heap env = undefined
instantiateAndUpdate (EAp e1 e2) upd_addr heap env =
  hUpdate heap2 upd_addr (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
{-    
instantiateAndUpdate (EConstr tag arity) upd_addr heap env = instantiateAndUpdateConstr tag arity heap env
instantiateAndUpdate (ELet isrec defs body) upd_addr heap env = instantiateAndUpdateLet isrec defs body upd_addr heap env
instantiateAndUpdate (ECase e alts) upd_addr heap env = error "Can't instantiate case exprs"
instantiateAndUpdateConstr tag arity update_addr heap env = error "Can't instantiate constructors yet"
instantiateAndUpdateLet = undefined    
-}


----- Helper functions for pPretty print
showResults::[TiState] -> String
showResults states =
  iDisplay (iConcat [iLayn (map showState states), showStats (last states)])

showState :: TiState ->Iseq
showState (stack, dump, heap, globals, tats) =
  iConcat ([showStack heap stack, iNewline] ++ showAddress heap)


showAddress:: Heap Node -> [Iseq]
showAddress hp@(Heap size _ cts) = [iStr $ show size, iNewline] ++ [showNodes hp]
  where
    showNodes = iInterleave iNewline . map (showNode . hLookup hp) . hAddresses



showStack:: TiHeap -> TiStack -> Iseq
showStack heap stack =
  iConcat[ iStr "Stk [",
           iIndent (iInterleave iNewline (map show_stack_item stack)),
           iStr " ]"]
  where
    show_stack_item addr =
      iConcat [ showFWAddr addr, iStr ": ",
                showStkNode heap (hLookup heap addr)]
      
showStkNode::TiHeap -> Node -> Iseq
showStkNode heap (NAp fun_addr arg_addr) =
  iConcat [ iStr "NAp ", showFWAddr fun_addr,
            iStr " ", showFWAddr arg_addr, iStr " (",
            showNode (hLookup heap arg_addr), iStr ")"]
showStkNode heap node = showNode node
showNode :: Node -> Iseq
showNode (NAp a1 a2) =  iConcat [ iStr "NAp ", showAddr a1, iStr " ", showAddr a2]
showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = (iStr "NNum ") `iAppend` (iNum n)
showNode (NInd a) = (iStr "NInd ") `iAppend` (iNum a)

showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)
showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where
    str = show addr

showStats::TiState -> Iseq
showStats (stack, dump, heap, globals, stats) =
  iConcat [iNewline, iNewline, iStr "Total number of steps = ",
           iNum (tiStatGetSteps stats)]


           
