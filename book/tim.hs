module Tim where
import Utils
import Language
{-exs_1-}intCode = []
runProg     :: [Char] -> [Char]
compile     :: CoreProgram -> TimState
eval        :: TimState -> [TimState]
showResults :: [TimState] -> [Char]

runProg = showResults . eval . compile . parse
fullRun :: [Char] -> [Char]
fullRun = showFullResults . eval . compile . parse
-- :a language.lhs  -- parser data types
{-exs_1-}data Instruction = Take Int
{-exs_1-}                 | Enter TimAMode
{-exs_1-}                 | Push TimAMode
{-exs_1-4-}data TimAMode = Arg Int
{-exs_1-4-}              | Label [Char]
{-exs_1-4-}              | Code [Instruction]
{-exs_1-4-}              | IntConst Int
{-exs_1-4-}type TimState = ([Instruction],        -- The current instruction stream
{-exs_1-4-}                 FramePtr,             -- Address of current frame
{-exs_1-4-}                 TimStack,             -- Stack of arguments
{-exs_1-4-}                 TimValueStack,        -- Value stack (not used yet)
{-exs_1-4-}                 TimDump,              -- Dump (not used yet)
{-exs_1-4-}                 TimHeap,              -- Heap of frames
{-exs_1-4-}                 CodeStore,            -- Labelled blocks of code
{-exs_1-4-}                 TimStats)             -- Statistics
data FramePtr = FrameAddr Addr         -- The address of a frame
              | FrameInt Int           -- An integer value
              | FrameNull              -- Uninitialised
type TimStack = [Closure]
type Closure = ([Instruction], FramePtr)
{-exs_1-}data TimValueStack = DummyTimValueStack
{-exs_1-3-}data TimDump = DummyTimDump
type TimHeap = Heap Frame

fAlloc   :: TimHeap -> [Closure] -> (TimHeap, FramePtr)
fGet     :: TimHeap -> FramePtr -> Int -> Closure
fUpdate  :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
fList    :: Frame -> [Closure]           -- Used when printing
type Frame = [Closure]

fAlloc heap xs = (heap', FrameAddr addr)
                 where
                 (heap', addr) = hAlloc heap xs

fGet heap (FrameAddr addr) n = f !! (n-1)
                               where
                               f = hLookup heap addr

fUpdate heap (FrameAddr addr) n closure
 = hUpdate heap addr new_frame
   where
   frame = hLookup heap addr
   new_frame = take (n-1) frame ++ [closure] ++ drop n frame

fList f = f
{-exs_1-5-}type CodeStore = ASSOC Name [Instruction]
{-exs_1-5-}codeLookup :: CodeStore -> Name -> [Instruction]
{-exs_1-5-}codeLookup cstore l
{-exs_1-5-} = aLookup cstore l (error ("Attempt to jump to unknown label "
{-exs_1-5-}                            ++ show l))
statInitial  :: TimStats
statIncSteps :: TimStats -> TimStats
statGetSteps :: TimStats -> Int
type TimStats = Int           -- The number of steps
statInitial = 0
statIncSteps s = s+1
statGetSteps s = s
-- :a util.lhs -- heap data type and other library functions
{-exs_1-4-}compile program
{-exs_1-4-}    = ([Enter (Label "main")],     -- Initial instructions
{-exs_1-4-}       FrameNull,                  -- Null frame pointer
{-exs_1-4-}       initialArgStack,            -- Argument stack
{-exs_1-4-}       initialValueStack,          -- Value stack
{-exs_1-4-}       initialDump,                -- Dump
{-exs_1-4-}       hInitial,                   -- Empty heap
{-exs_1-4-}       compiled_code,              -- Compiled code for supercombinators
{-exs_1-4-}       statInitial)                -- Initial statistics
{-exs_1-4-}       where
{-exs_1-4-}       sc_defs          = preludeDefs ++ program
{-exs_1-4-}       compiled_sc_defs = map (compileSC initial_env) sc_defs
{-exs_1-4-}       compiled_code    = compiled_sc_defs ++ compiledPrimitives
{-exs_1-4-}       initial_env = [(name, Label name) | (name, args, body) <- sc_defs]
{-exs_1-4-}			  ++ [(name, Label name) | (name, code) <- compiledPrimitives]
{-exs_1-}initialArgStack = []
{-exs_1-}initialValueStack = DummyTimValueStack
{-exs_1-3-}initialDump = DummyTimDump
compiledPrimitives = []
type TimCompilerEnv = [(Name, TimAMode)]
compileSC :: TimCompilerEnv -> CoreScDefn -> (Name, [Instruction])
{-exs_1-2-}compileSC env (name, args, body)
{-exs_1-2-} = (name, Take (length args) : instructions)
{-exs_1-2-}    where
{-exs_1-2-}    instructions = compileR body new_env
{-exs_1-2-}    new_env = (zip2 args (map Arg [1..])) ++ env
{-exs_1-2-}compileR :: CoreExpr -> TimCompilerEnv -> [Instruction]
{-exs_1-}compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
{-exs_1-}compileR (EVar v)    env = [Enter (compileA (EVar v) env)]
{-exs_1-}compileR (ENum n)    env = [Enter (compileA (ENum n) env)]
{-exs_1-}compileR e           env = error "compileR: can't do this yet"
{-exs_1-2-}compileA :: CoreExpr -> TimCompilerEnv -> TimAMode
{-exs_1-2-}compileA (EVar v) env = aLookup env v (error ("Unknown variable " ++ v))
{-exs_1-2-}compileA (ENum n) env = IntConst n
{-exs_1-2-}compileA e        env = Code (compileR e env)
eval state
 = state : rest_states  where
                        rest_states | timFinal state = []
                                    | otherwise      = eval next_state
                        next_state  = doAdmin (step state)

doAdmin state = applyToStats statIncSteps state
{-exs_1-4-}timFinal ([], frame, stack, vstack, dump, heap, cstore, stats) = True
{-exs_1-4-}timFinal state                                                 = False
{-exs_1-4-}applyToStats stats_fun (instr, frame, stack, vstack,
{-exs_1-4-}                        dump, heap, cstore, stats)
{-exs_1-4-} = (instr, frame, stack, vstack, dump, heap, cstore, stats_fun stats)
{-exs_1-}step ((Take n:instr), fptr, stack, vstack, dump, heap, cstore,stats)
{-exs_1-} | length stack >= n = (instr, fptr', drop n stack, vstack, dump, heap', cstore, stats)
{-exs_1-} | otherwise         = error "Too few args for Take instruction"
{-exs_1-}   where (heap', fptr') = fAlloc heap (take n stack)
{-exs_1-}step ([Enter am], fptr, stack, vstack, dump, heap, cstore, stats)
{-exs_1-} = (instr', fptr', stack, vstack, dump, heap, cstore, stats)
{-exs_1-}   where (instr',fptr') = amToClosure am fptr heap cstore
{-exs_1-}step ((Push am:instr), fptr, stack, vstack, dump, heap, cstore, stats)
{-exs_1-} = (instr, fptr, amToClosure am fptr heap cstore : stack,
{-exs_1-}    vstack, dump, heap, cstore, stats)
{-exs_1-4-}amToClosure :: TimAMode -> FramePtr -> TimHeap -> CodeStore -> Closure
{-exs_1-4-}amToClosure (Arg n)      fptr heap cstore = fGet heap fptr n
{-exs_1-4-}amToClosure (Code il)    fptr heap cstore = (il, fptr)
{-exs_1-4-}amToClosure (Label l)    fptr heap cstore = (codeLookup cstore l, fptr)
{-exs_1-4-}amToClosure (IntConst n) fptr heap cstore = (intCode, FrameInt n)
showFullResults states
 = iDisplay (iConcat [
       iStr "Supercombinator definitions", iNewline, iNewline,
       showSCDefns first_state, iNewline, iNewline,
       iStr "State transitions", iNewline,
       iLayn (map showState states), iNewline, iNewline,
       showStats (last states)
   ])
   where
   (first_state:rest_states) = states
{-exs_1-4-}showResults states
{-exs_1-4-} = iDisplay (iConcat [
{-exs_1-4-}    showState last_state, iNewline, iNewline, showStats last_state
{-exs_1-4-}   ])
{-exs_1-4-}   where last_state = last states
showSCDefns :: TimState -> Iseq
{-exs_1-4-}showSCDefns (instr, fptr, stack, vstack, dump, heap, cstore, stats)
{-exs_1-4-} = iInterleave iNewline (map showSC cstore)
showSC :: (Name, [Instruction]) -> Iseq
showSC (name, il)
 = iConcat [
       iStr "Code for ", iStr name, iStr ":", iNewline,
       iStr "   ", showInstructions Full il, iNewline, iNewline
   ]
showState :: TimState -> Iseq
{-exs_1-4-}showState (instr, fptr, stack, vstack, dump, heap, cstore, stats)
{-exs_1-4-} = iConcat [
{-exs_1-4-}    iStr "Code:  ", showInstructions Terse instr, iNewline,
{-exs_1-4-}    showFrame heap fptr,
{-exs_1-4-}    showStack stack,
{-exs_1-4-}    showValueStack vstack,
{-exs_1-4-}    showDump dump,
{-exs_1-4-}    iNewline
{-exs_1-4-}   ]
showFrame :: TimHeap -> FramePtr -> Iseq
showFrame heap FrameNull = iStr "Null frame ptr" `iAppend` iNewline
showFrame heap (FrameAddr addr)
 = iConcat [
       iStr "Frame: <",
       iIndent (iInterleave iNewline
                            (map showClosure (fList (hLookup heap addr)))),
       iStr ">", iNewline
   ]
showFrame heap (FrameInt n)
 = iConcat [ iStr "Frame ptr (int): ", iNum n, iNewline ]
showStack :: TimStack -> Iseq
showStack stack
 = iConcat [   iStr "Arg stack: [",
               iIndent (iInterleave iNewline (map showClosure stack)),
               iStr "]", iNewline
   ]
showValueStack :: TimValueStack -> Iseq
{-exs_1-}showValueStack vstack = iNil
showDump :: TimDump -> Iseq
{-exs_1-3-}showDump dump = iNil
showClosure :: Closure -> Iseq
showClosure (i,f)
 = iConcat [   iStr "(",  showInstructions Terse i,  iStr ", ",
               showFramePtr f,  iStr ")"
   ]
showFramePtr :: FramePtr -> Iseq
showFramePtr FrameNull = iStr "null"
showFramePtr (FrameAddr a) = iStr (show a)
showFramePtr (FrameInt n) = iStr "int " `iAppend` iNum n
showStats :: TimState -> Iseq
{-exs_1-4-}showStats (instr, fptr, stack, vstack, dump, heap, code, stats)
{-exs_1-4-} = iConcat [ iStr "Steps taken = ", iNum (statGetSteps stats), iNewline,
{-exs_1-4-}             iStr "No of frames allocated = ", iNum (hSize heap),
{-exs_1-4-}             iNewline
{-exs_1-4-}   ]
data HowMuchToPrint = Full | Terse | None
showInstructions :: HowMuchToPrint -> [Instruction] -> Iseq
showInstructions None il = iStr "{..}"
showInstructions Terse il
 = iConcat [iStr "{", iIndent (iInterleave (iStr ", ") body), iStr "}"]
   where
      instrs = map (showInstruction None) il
      body | length il <= nTerse = instrs
           | otherwise           = (take nTerse instrs) ++ [iStr ".."]
showInstructions Full il
 = iConcat [iStr "{ ", iIndent (iInterleave sep instrs), iStr " }"]
   where
   sep = iStr "," `iAppend` iNewline
   instrs = map (showInstruction Full) il
{-exs_1-}showInstruction d (Take m)  = (iStr "Take ")  `iAppend` (iNum m)
{-exs_1-}showInstruction d (Enter x) = (iStr "Enter ") `iAppend` (showArg d x)
{-exs_1-}showInstruction d (Push x)  = (iStr "Push ")  `iAppend` (showArg d x)
{-exs_1-4-}showArg d (Arg m)      = (iStr "Arg ")   `iAppend` (iNum m)
{-exs_1-4-}showArg d (Code il)    = (iStr "Code ")  `iAppend` (showInstructions d il)
{-exs_1-4-}showArg d (Label s)    = (iStr "Label ") `iAppend` (iStr s)
{-exs_1-4-}showArg d (IntConst n) = (iStr "IntConst ") `iAppend` (iNum n)
nTerse = 3
{-exs_2--}intCode = [PushV FramePtr, Return]
{-exs_2--}type TimValueStack = [Int]
{-exs_2--}initialValueStack = []
{-exs_2-}data Instruction = Take Int
{-exs_2-}                 | Push TimAMode
{-exs_2-}                 | PushV ValueAMode
{-exs_2-}                 | Enter TimAMode
{-exs_2-}                 | Return
{-exs_2-}                 | Op Op
{-exs_2-}                 | Cond [Instruction] [Instruction]
{-exs_2--}data Op = Add  | Sub | Mult | Div | Neg
{-exs_2--}        | Gr | GrEq | Lt | LtEq | Eq | NotEq
{-exs_2--}      deriving (Eq) -- KH
{-exs_2--}data ValueAMode = FramePtr
{-exs_2--}                | IntVConst Int
{-exs_2--}initialArgStack = [([], FrameNull)]
mkIndMode :: Int -> TimAMode
mkIndMode n = Code [Enter (Arg n)]
{-exs_4--}type TimDump = [(FramePtr,  -- The frame to be updated
{-exs_4--}                 Int,       -- Index of slot to be updated
{-exs_4--}                 TimStack)  -- Old stack
{-exs_4--}               ]
{-exs_4--}initialDump = []
{-exs_4--}mkUpdIndMode :: Int -> TimAMode
{-exs_4--}mkUpdIndMode n = Code [PushMarker n, Enter (Arg n)]
mkEnter :: TimAMode -> [Instruction]
mkEnter (Code i) = i
mkEnter other_am = [Enter other_am]
{-exs_6-}type CodeStore = (Addr, ASSOC Name Int)
{-exs_6-}allocateInitialHeap :: [(Name, [Instruction])] -> (TimHeap, CodeStore)
{-exs_6-}allocateInitialHeap compiled_code
{-exs_6-} = (heap, (global_frame_addr, offsets))
{-exs_6-}   where
{-exs_6-}   indexed_code = zip2 [1..] compiled_code
{-exs_6-}   offsets = [(name, offset) | (offset, (name, code)) <- indexed_code]
{-exs_6-}   closures = [(PushMarker offset : code, global_frame_addr) |
{-exs_6-}                      (offset, (name, code)) <- indexed_code]
{-exs_6-}   (heap, global_frame_addr) = fAlloc hInitial closures
