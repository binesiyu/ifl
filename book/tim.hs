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
data FramePtr = FrameAddr Addr         -- The address of a frame
              | FrameInt Int           -- An integer value
              | FrameNull              -- Uninitialised
type TimStack = [Closure]
type Closure = ([Instruction], FramePtr)
{-exs_1-}data TimValueStack = DummyTimValueStack
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
statInitial  :: TimStats
statIncSteps :: TimStats -> TimStats
statGetSteps :: TimStats -> Int
type TimStats = Int           -- The number of steps
statInitial = 0
statIncSteps s = s+1
statGetSteps s = s
-- :a util.lhs -- heap data type and other library functions
{-exs_1-}initialArgStack = []
{-exs_1-}initialValueStack = DummyTimValueStack
compiledPrimitives = []
type TimCompilerEnv = [(Name, TimAMode)]
compileSC :: TimCompilerEnv -> CoreScDefn -> (Name, [Instruction])
{-exs_1-}compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
{-exs_1-}compileR (EVar v)    env = [Enter (compileA (EVar v) env)]
{-exs_1-}compileR (ENum n)    env = [Enter (compileA (ENum n) env)]
{-exs_1-}compileR e           env = error "compileR: can't do this yet"
eval state
 = state : rest_states  where
                        rest_states | timFinal state = []
                                    | otherwise      = eval next_state
                        next_state  = doAdmin (step state)

doAdmin state = applyToStats statIncSteps state
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
showSCDefns :: TimState -> Iseq
showSC :: (Name, [Instruction]) -> Iseq
showSC (name, il)
 = iConcat [
       iStr "Code for ", iStr name, iStr ":", iNewline,
       iStr "   ", showInstructions Full il, iNewline, iNewline
   ]
showState :: TimState -> Iseq
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
nTerse = 3
{-exs_2-}data Instruction = Take Int
{-exs_2-}                 | Push TimAMode
{-exs_2-}                 | PushV ValueAMode
{-exs_2-}                 | Enter TimAMode
{-exs_2-}                 | Return
{-exs_2-}                 | Op Op
{-exs_2-}                 | Cond [Instruction] [Instruction]
mkIndMode :: Int -> TimAMode
mkIndMode n = Code [Enter (Arg n)]
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
