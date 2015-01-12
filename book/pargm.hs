module ParGM where
import Utils
import Language
--import GM
type PgmState = (PgmGlobalState,    -- Current global state
             [PgmLocalState])    	-- Current states of processors
type PgmGlobalState = (GmOutput,          -- output stream
                   GmHeap,            -- Heap of nodes
                   GmGlobals,         -- Global addresses in heap
                   GmSparks,          -- Sparked task pool
                   GmStats)           -- Statistics
type GmOutput = [Char]
pgmGetOutput :: PgmState -> GmOutput
pgmGetOutput ((o, heap, globals, sparks, stats), locals) = o
type GmHeap = Heap Node
pgmGetHeap :: PgmState -> GmHeap
pgmGetHeap ((o, heap, globals, sparks, stats), locals) = heap
type GmGlobals = ASSOC Name Addr
pgmGetGlobals :: PgmState -> GmGlobals
pgmGetGlobals ((o, heap, globals, sparks, stats), locals) = globals
pgmGetSparks :: PgmState -> GmSparks
pgmGetSparks ((o, heap, globals, sparks, stats), locals) = sparks
type GmStats = [Int]
pgmGetStats :: PgmState -> GmStats
pgmGetStats ((o, heap, globals, sparks, stats), locals) = stats
type GmCode = [Instruction]
type GmStack = [Addr]
type GmVStack = [Int]
type GmClock = Int
type GmState = (PgmGlobalState, PgmLocalState)
putOutput :: GmOutput -> GmState -> GmState
putHeap :: GmHeap   -> GmState -> GmState
putSparks :: GmSparks -> GmState -> GmState
putStats :: GmStats  -> GmState -> GmState
getOutput :: GmState  -> GmOutput
getHeap :: GmState  -> GmHeap
getGlobals :: GmState  -> GmGlobals
getSparks :: GmState  -> GmSparks
getStats :: GmState  -> GmStats
putCode :: GmCode   -> GmState -> GmState
putStack :: GmStack  -> GmState -> GmState
putDump :: GmDump   -> GmState -> GmState
putVStack :: GmVStack -> GmState -> GmState
putClock :: GmClock  -> GmState -> GmState
getCode :: GmState  -> GmCode
getStack :: GmState  -> GmStack
getDump :: GmState  -> GmDump
getVStack :: GmState  -> GmVStack
getClock :: GmState  -> GmClock
eval :: PgmState -> [PgmState]
eval state = state: restStates
             where
             restStates | gmFinal state  = []
                        | otherwise = eval (doAdmin (steps state))
makeTask :: Addr -> PgmLocalState
tick (i, stack, dump, vstack, clock) = (i, stack, dump, vstack, clock+1)
gmFinal :: PgmState -> Bool
gmFinal s = second s == [] && pgmGetSparks s == []
step :: PgmGlobalState -> PgmLocalState -> GmState
step global local = dispatch i (putCode is state)
                    where (i:is) = getCode state
                          state = (global, local)
doAdmin :: PgmState -> PgmState
par :: GmState -> GmState
compile :: CoreProgram -> PgmState
compile program
 = (([], heap, globals, [], []), [initialTask addr])
   where (heap, globals) = buildInitialHeap program
         addr            = aLookup globals "main" (error "main undefined")
initialTask :: Addr -> PgmLocalState
initialTask addr = (initialCode, [addr], [], [], 0)
initialCode :: GmCode
showResults :: [PgmState] -> [Char]
showSC :: PgmState -> (Name, Addr) -> Iseq
showState :: PgmState -> Iseq
showStats :: PgmState -> Iseq
showOutput :: GmOutput -> Iseq
showSparks :: GmSparks -> Iseq
