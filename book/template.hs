module Template where
import Language
import Utils
type MultState = (Int, Int, Int, Int)     -- (n, m, d, t)
evalMult :: MultState -> [MultState]
evalMult state = if multFinal state 
                   then [state]
                   else state : evalMult (stepMult state)
stepMult (n, m, d, t) | d > 0  = (n, m,   d-1, t+1)
stepMult (n, m, d, t) | d == 0 = (n, m-1, n,   t)
multFinal :: MultState -> Bool
runProg :: [Char] -> [Char]      -- name changed to not conflict
compile :: CoreProgram -> TiState
eval :: TiState -> [TiState]
showResults :: [TiState] -> [Char]
runProg = showResults . eval . compile . parse  -- "run": name conflict
-- :a language.lhs
-- import Language
-- :a utils.lhs
-- import Utils
type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]
type TiHeap = Heap Node
type TiGlobals = ASSOC Name Addr
tiStatInitial  :: TiStats
tiStatIncSteps :: TiStats -> TiStats
tiStatGetSteps :: TiStats -> Int
type TiStats = Int
tiStatInitial    = 0
tiStatIncSteps s = s+1
tiStatGetSteps s = s
applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats)
 = (stack, dump, heap, sc_defs, stats_fun stats)
compile program
 = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
   where
   sc_defs = program ++ preludeDefs ++ extraPreludeDefs

   (initial_heap, globals) = buildInitialHeap sc_defs

   initial_stack = [address_of_main]
   address_of_main = aLookup globals "main" (error "main is not defined")
allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body)
 = (heap', (name, addr))
   where
   (heap', addr) = hAlloc heap (NSupercomb name args body)
eval state = state : rest_states
             where
             rest_states | tiFinal state = []
                         | otherwise = eval next_state
             next_state  = doAdmin (step state)
doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state
step :: TiState -> TiState
-- now getargs since getArgs conflicts with Gofer standard.prelude
getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack)
 = map get_arg stack
   where get_arg addr = arg  where (NAp fun arg) = hLookup heap addr
instantiate :: CoreExpr              -- Body of supercombinator
               -> TiHeap             -- Heap before instantiation
               -> ASSOC Name Addr    -- Association of names to addresses
               -> (TiHeap, Addr)     -- Heap after instantiation, and
                                     -- address of root of instance
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env
 = hAlloc heap2 (NAp a1 a2) where (heap1, a1) = instantiate e1 heap  env
                                  (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env
 = (heap, aLookup env v (error ("Undefined name " ++ show v)))
instantiate (EConstr tag arity) heap env
              = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env
              = instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = error "Can't instantiate case exprs"
instantiateLet isrec defs body heap env
           = error "Can't instantiate let(rec)s yet"
showResults states
 = iDisplay (iConcat [ iLayn (map showState states),
                     showStats (last states)
          ])
showState :: TiState -> Iseq
showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack
 = iConcat [
       iStr "Stk [",
       iIndent (iInterleave iNewline (map show_stack_item stack)),
       iStr " ]"
   ]
   where
   show_stack_item addr
    = iConcat [ showFWAddr addr, iStr ": ",
                showStkNode heap (hLookup heap addr)
      ]
showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fun_addr arg_addr)
 = iConcat [   iStr "NAp ", showFWAddr fun_addr,
               iStr " ", showFWAddr arg_addr, iStr " (",
               showNode (hLookup heap arg_addr), iStr ")"
   ]
showStkNode heap node = showNode node
showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq    -- Show address in field of width 4
showFWAddr addr = iStr (space (4 -  length str) ++ str)
                  where
                  str = show addr
showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats)
 = iConcat [ iNewline, iNewline, iStr "Total number of steps = ",
             iNum (tiStatGetSteps stats)
   ]
data Node = NAp Addr Addr                      -- Application
           | NSupercomb Name [Name] CoreExpr   -- Supercombinator
           | NNum Int                          -- Number
           | NInd Addr                         -- Indirection
data Node = NAp Addr Addr                       -- Application
            | NSupercomb Name [Name] CoreExpr   -- Supercombinator
            | NNum Int                          -- Number
            | NInd Addr                         -- Indirection
            | NPrim Name Primitive              -- Primitive
data Primitive = Neg | Add | Sub | Mul | Div
primitives :: ASSOC Name Primitive
primitives = [ ("negate", Neg),
               ("+", Add),   ("-", Sub),
               ("*", Mul),   ("/", Div)
             ]
primStep state Neg   = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state (div)
primArith :: TiState -> (Int -> Int -> Int) -> TiState
data Node = NAp Addr Addr                     -- Application
          | NSupercomb Name [Name] CoreExpr   -- Supercombinator
          | NNum Int                          -- Number
          | NInd Addr                         -- Indirection
          | NPrim Name Primitive              -- Primitive
          | NData Int [Addr]                  -- Tag, list of components
findStackRoots  :: TiStack -> [Addr]
findDumpRoots   :: TiDump -> [Addr]
findGlobalRoots :: TiGlobals -> [Addr]
markFrom :: TiHeap -> Addr -> TiHeap
scanHeap :: TiHeap -> TiHeap
data Node = NAp Addr Addr                     -- Application
          | NSupercomb Name [Name] CoreExpr   -- Supercombinator
          | NNum Int                          -- Number
          | NInd Addr                         -- Indirection
          | NPrim Name Primitive              -- Primitive
          | NData Int [Addr]                  -- Tag, list of components
          | NMarked Node                      -- Marked node
markFrom :: TiHeap -> Addr -> (TiHeap, Addr)
markFromStack   :: TiHeap -> TiStack   -> (TiHeap,TiStack)
markFromDump    :: TiHeap -> TiDump    -> (TiHeap,TiDump)
markFromGlobals :: TiHeap -> TiGlobals -> (TiHeap,TiGlobals)
data Node = NAp Addr Addr                     -- Application
          | NSupercomb Name [Name] CoreExpr   -- Supercombinator
          | NNum Int                          -- Number
          | NInd Addr                         -- Indirection
          | NPrim Name Primitive              -- Primitive
          | NData Int [Addr]                  -- Tag, list of components
          | NMarked MarkState Node            -- Marked node
data markState = Done         -- Marking on this node finished
               | Visits Int   -- Node visited n times so far
evacuateStack :: TiHeap -> TiHeap -> TiStack -> (TiHeap, TiStack)
scavengeHeap :: TiHeap -> TiHeap -> TiHeap
