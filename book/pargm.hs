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
{-exs_1-3-}type GmSparks = [Addr]
pgmGetSparks :: PgmState -> GmSparks
pgmGetSparks ((o, heap, globals, sparks, stats), locals) = sparks
type GmStats = [Int]
pgmGetStats :: PgmState -> GmStats
pgmGetStats ((o, heap, globals, sparks, stats), locals) = stats
{-exs_1-4-}type PgmLocalState = (GmCode,    -- Instruction stream
{-exs_1-4-}                  GmStack,       -- Pointer stack
{-exs_1-4-}                  GmDump,        -- Stack of dump items
{-exs_1-4-}                  GmVStack,      -- Value stack
{-exs_1-4-}                  GmClock)       -- Number of ticks the task
{-exs_1-4-}                                 --          has been active
type GmCode = [Instruction]
type GmStack = [Addr]
{-exs_1-4-}type GmDump     = [GmDumpItem]
{-exs_1-4-}type GmDumpItem = (GmCode, GmStack)
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
{-exs_1-2-}steps :: PgmState -> PgmState
{-exs_1-2-}steps state
{-exs_1-2-} = mapAccuml step global' local'
{-exs_1-2-}   where ((out, heap, globals, sparks, stats), local) = state
{-exs_1-2-}         newtasks = [makeTask a | a <- sparks]
{-exs_1-2-}         global'  = (out, heap, globals, [], stats)
{-exs_1-2-}         local'   = map tick (local ++ newtasks)
makeTask :: Addr -> PgmLocalState
tick (i, stack, dump, vstack, clock) = (i, stack, dump, vstack, clock+1)
gmFinal :: PgmState -> Bool
gmFinal s = second s == [] && pgmGetSparks s == []
step :: PgmGlobalState -> PgmLocalState -> GmState
step global local = dispatch i (putCode is state)
                    where (i:is) = getCode state
                          state = (global, local)
doAdmin :: PgmState -> PgmState
{-exs_1-3-}doAdmin ((out, heap, globals, sparks, stats), local)
{-exs_1-3-} = ((out, heap, globals, sparks, stats'), local')
{-exs_1-3-}   where (local', stats') = foldr filter ([], stats) local
{-exs_1-3-}         filter (i, stack, dump, vstack, clock) (local, stats)
{-exs_1-3-}          | i == [] = (local, clock:stats)
{-exs_1-3-}          | otherwise = ((i, stack, dump, vstack, clock): local, stats)
par :: GmState -> GmState
{-exs_1-3-}par s = s
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
{-exs_2-3-}data Node = NNum Int            -- Numbers
{-exs_2-3-}         | NAp  Addr Addr       -- Applications
{-exs_2-3-}         | NGlobal Int GmCode   -- Globals
{-exs_2-3-}         | NInd Addr            -- Indirections
{-exs_2-3-}         | NConstr Int [Addr]   -- Constructors
{-exs_2-3-}         | NLAp Addr Addr       -- Locked applications
{-exs_2-3-}         | NLGlobal Int GmCode  -- Locked globals
{-exs_2-3-}lock :: Addr -> GmState -> GmState
{-exs_2-3-}lock addr state
{-exs_2-3-} = putHeap (newHeap (hLookup heap addr)) state
{-exs_2-3-}   where
{-exs_2-3-}   heap = getHeap state
{-exs_2-3-}   newHeap (NAp a1 a2)   = hUpdate heap addr (NLAp a1 a2)
{-exs_2-3-}   newHeap (NGlobal n c) | n == 0 = hUpdate heap addr (NLGlobal n c)
{-exs_2-3-}                         | otherwise = heap
{-exs_2-3-}unlock :: Addr -> GmState -> GmState
{-exs_2-3-}unlock addr state
{-exs_2-3-} = newState (hLookup heap addr)
{-exs_2-3-}   where
{-exs_2-3-}   heap = getHeap state
{-exs_2-3-}   newState (NLAp a1 a2)
{-exs_2-3-}     = unlock a1 (putHeap (hUpdate heap addr (NAp a1 a2)) state)
{-exs_2-3-}   newState (NLGlobal n c)
{-exs_2-3-}     = putHeap (hUpdate heap addr (NGlobal n c)) state
{-exs_2-3-}   newState n = state
{-exs_2-3-}getArg (NLAp a1 a2) = a2
{-exs_3--}machineSize :: Int
{-exs_3--}machineSize = 4
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
