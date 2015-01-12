module Template where
import Language
import Utils
{-exs_1-}type MultState = (Int, Int, Int, Int)     -- (n, m, d, t)
{-exs_1-}evalMult :: MultState -> [MultState]
{-exs_1-}evalMult state = if multFinal state 
{-exs_1-}                   then [state]
{-exs_1-}                   else state : evalMult (stepMult state)
{-exs_1-}stepMult (n, m, d, t) | d > 0  = (n, m,   d-1, t+1)
{-exs_1-}stepMult (n, m, d, t) | d == 0 = (n, m-1, n,   t)
{-exs_1-}multFinal :: MultState -> Bool
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
{-exs_1-}instantiateLet isrec defs body heap env
{-exs_1-}           = error "Can't instantiate let(rec)s yet"
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
{-exs_3-}data Node = NAp Addr Addr                      -- Application
{-exs_3-}           | NSupercomb Name [Name] CoreExpr   -- Supercombinator
{-exs_3-}           | NNum Int                          -- Number
{-exs_3-}           | NInd Addr                         -- Indirection
{-exs_4-}data Node = NAp Addr Addr                       -- Application
{-exs_4-}            | NSupercomb Name [Name] CoreExpr   -- Supercombinator
{-exs_4-}            | NNum Int                          -- Number
{-exs_4-}            | NInd Addr                         -- Indirection
{-exs_4-}            | NPrim Name Primitive              -- Primitive
{-exs_4-}data Primitive = Neg | Add | Sub | Mul | Div
{-exs_4-}primitives :: ASSOC Name Primitive
{-exs_4-}primitives = [ ("negate", Neg),
{-exs_4-}               ("+", Add),   ("-", Sub),
{-exs_4-}               ("*", Mul),   ("/", Div)
{-exs_4-}             ]
{-exs_4-}primStep state Neg   = primNeg state
{-exs_4-}primStep state Add = primArith state (+)
{-exs_4-}primStep state Sub = primArith state (-)
{-exs_4-}primStep state Mul = primArith state (*)
{-exs_4-}primStep state Div = primArith state (div)
{-exs_4-}primArith :: TiState -> (Int -> Int -> Int) -> TiState
{-exs_5-}data Node = NAp Addr Addr                     -- Application
{-exs_5-}          | NSupercomb Name [Name] CoreExpr   -- Supercombinator
{-exs_5-}          | NNum Int                          -- Number
{-exs_5-}          | NInd Addr                         -- Indirection
{-exs_5-}          | NPrim Name Primitive              -- Primitive
{-exs_5-}          | NData Int [Addr]                  -- Tag, list of components
{-exs_6-}findStackRoots  :: TiStack -> [Addr]
{-exs_6-}findDumpRoots   :: TiDump -> [Addr]
{-exs_6-}findGlobalRoots :: TiGlobals -> [Addr]
{-exs_6-}markFrom :: TiHeap -> Addr -> TiHeap
{-exs_6-}scanHeap :: TiHeap -> TiHeap
{-exs_6-}data Node = NAp Addr Addr                     -- Application
{-exs_6-}          | NSupercomb Name [Name] CoreExpr   -- Supercombinator
{-exs_6-}          | NNum Int                          -- Number
{-exs_6-}          | NInd Addr                         -- Indirection
{-exs_6-}          | NPrim Name Primitive              -- Primitive
{-exs_6-}          | NData Int [Addr]                  -- Tag, list of components
{-exs_6-}          | NMarked Node                      -- Marked node
{-exs_7-}markFrom :: TiHeap -> Addr -> (TiHeap, Addr)
{-exs_7-}markFromStack   :: TiHeap -> TiStack   -> (TiHeap,TiStack)
{-exs_7-}markFromDump    :: TiHeap -> TiDump    -> (TiHeap,TiDump)
{-exs_7-}markFromGlobals :: TiHeap -> TiGlobals -> (TiHeap,TiGlobals)
{-exs_8-}data Node = NAp Addr Addr                     -- Application
{-exs_8-}          | NSupercomb Name [Name] CoreExpr   -- Supercombinator
{-exs_8-}          | NNum Int                          -- Number
{-exs_8-}          | NInd Addr                         -- Indirection
{-exs_8-}          | NPrim Name Primitive              -- Primitive
{-exs_8-}          | NData Int [Addr]                  -- Tag, list of components
{-exs_8-}          | NMarked MarkState Node            -- Marked node
{-exs_8-}data markState = Done         -- Marking on this node finished
{-exs_8-}               | Visits Int   -- Node visited n times so far
{-exs_9-}evacuateStack :: TiHeap -> TiHeap -> TiStack -> (TiHeap, TiStack)
{-exs_9-}scavengeHeap :: TiHeap -> TiHeap -> TiHeap
