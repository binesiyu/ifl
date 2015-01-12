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
{-exs_3-}steps :: PgmState -> PgmState
{-exs_3-}steps state
{-exs_3-} = scheduler global' local'
{-exs_3-}   where ((out, heap, globals, sparks, stats), local) = state
{-exs_3-}         newtasks = [makeTask a | a <- sparks]
{-exs_3-}         global'  = (out, heap, globals, [], stats)
{-exs_3-}         local'   = local ++ newtasks
{-exs_3-}scheduler :: PgmGlobalState -> [PgmLocalState] -> PgmState
{-exs_3-}scheduler global tasks
{-exs_3-} = (global', nonRunning ++ tasks')
{-exs_3-}   where running    = map tick (take machineSize tasks)
{-exs_3-}         nonRunning = drop machineSize tasks
{-exs_3-}         (global', tasks') = mapAccuml step global running
{-exs_4-}data Node = NNum Int                          -- Numbers
{-exs_4-}         | NAp  Addr Addr                     -- Applications
{-exs_4-}         | NGlobal Int GmCode                 -- Globals
{-exs_4-}         | NInd Addr                          -- Indirections
{-exs_4-}         | NConstr Int [Addr]                 -- Constructors
{-exs_4-}         | NLAp Addr Addr PgmPendingList      -- Locked applications
{-exs_4-}         | NLGlobal Int GmCode PgmPendingList -- Locked globals
{-exs_4-}type PgmPendingList = [PgmLocalState]
{-exs_4-}type GmSparks = [PgmLocalState]
{-exs_4-}emptyPendingList :: [PgmLocalState] -> GmState -> GmState
{-exs_4-}emptyPendingList tasks state
{-exs_4-} = putSparks (tasks ++ getSparks state) state
{-exs_4-}emptyTask :: PgmLocalState
{-exs_4-}emptyTask = ([], [], [], [], 0)
{-exs_4-}getArg (NLAp a1 a2 pl) = a2
