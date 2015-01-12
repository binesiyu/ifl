module GM where
import Language
import Utils
-- The function run is already defined in gofers standard.prelude
runProg :: [Char] -> [Char]
runProg = showResults . eval . compile . parse
-- :a language.lhs -- parser data types
-- :a util.lhs -- heap data type and other library functions
type GmCode = [Instruction]
getCode :: GmState -> GmCode
putCode :: GmCode -> GmState -> GmState
data Instruction 
    = Unwind
    | Pushglobal Name
    | Pushint Int
    | Push Int
    | Mkap
    | Slide Int
instance Eq Instruction 
    where
    Unwind          == Unwind               = True
    Pushglobal a    == Pushglobal b         = a == b
    Pushint a       == Pushint b            = a == b
    Push a          == Push b               = a == b
    Mkap            == Mkap                 = True
    Slide a         == Slide b              = a == b
    _               == _                    = False
type GmStack = [Addr]
getStack :: GmState -> GmStack
putStack :: GmStack -> GmState -> GmState
type GmHeap = Heap Node
getHeap :: GmState -> GmHeap
putHeap :: GmHeap -> GmState -> GmState
data Node 
	= NNum Int		-- Numbers
	| NAp Addr Addr		-- Applications
	| NGlobal Int GmCode 	-- Globals
type GmGlobals = ASSOC Name Addr
getGlobals :: GmState -> GmGlobals
statInitial  :: GmStats
statIncSteps :: GmStats -> GmStats
statGetSteps :: GmStats -> Int
type GmStats = Int
statInitial    = 0
statIncSteps s = s+1
statGetSteps s = s
getStats :: GmState -> GmStats
putStats :: GmStats -> GmState -> GmState
eval :: GmState -> [GmState]
eval state = state: restStates
             where
             restStates | gmFinal state	= []
                        | otherwise		= eval nextState
             nextState  = doAdmin (step state)
doAdmin :: GmState -> GmState
doAdmin s = putStats (statIncSteps (getStats s)) s
gmFinal :: GmState -> Bool
gmFinal s = case (getCode s) of
                   []        -> True
                   otherwise -> False
step :: GmState -> GmState
dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n)    = pushint n
dispatch Mkap           = mkap
dispatch (Push n)       = push n
dispatch (Slide n)      = slide n
dispatch Unwind         = unwind
pushglobal :: Name -> GmState -> GmState
pushglobal f state
	= putStack (a: getStack state) state
  	where a = aLookup (getGlobals state) f (error ("Undeclared global " ++ f))
pushint :: Int -> GmState -> GmState
pushint n state
	= putHeap heap' (putStack (a: getStack state) state)
  	where (heap', a) = hAlloc (getHeap state) (NNum n)
mkap :: GmState -> GmState
mkap state
	= putHeap heap' (putStack (a:as') state)
      where (heap', a)  = hAlloc (getHeap state) (NAp a1 a2)
            (a1:a2:as') = getStack state
getArg :: Node -> Addr
getArg (NAp a1 a2) = a2
slide :: Int -> GmState -> GmState
slide n state
	= putStack (a: drop n as) state
  	where (a:as) = getStack state
unwind :: GmState -> GmState
unwind state
	= newState (hLookup heap a)
  	where
  		(a:as) = getStack state
  		heap   = getHeap state
  		newState (NNum n)      = state
  		newState (NAp a1 a2)   = putCode [Unwind] (putStack (a1:a:as) state)
 	          newState (NGlobal n c) 
 		         | length as < n	= error "Unwinding with too few arguments"
		         | otherwise	= putCode c state
type GmCompiledSC = (Name, Int, GmCode)
allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns)
	= (heap', (name, addr))
  	where (heap', addr) = hAlloc heap (NGlobal nargs instns)
compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body)
	= (name, length env, compileR body (zip2 env [0..]))
compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]
compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]
type GmCompiler = CoreExpr -> GmEnvironment -> GmCode
type GmEnvironment = ASSOC Name Int
argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v,m) <- env]
showResults :: [GmState] -> [Char]
showResults states
	= iDisplay (iConcat [
      iStr "Supercombinator definitions", iNewline,
      iInterleave iNewline (map (showSC s) (getGlobals s)),
      iNewline, iNewline, iStr "State transitions", iNewline, iNewline,
      iLayn (map showState states),
      iNewline, iNewline,
      showStats (last states)])
      where (s:ss) = states
showSC :: GmState -> (Name, Addr) -> Iseq
showSC s (name, addr)
	= iConcat [ iStr "Code for ", iStr name, iNewline,
            showInstructions code, iNewline, iNewline]
  	where (NGlobal arity code) = (hLookup (getHeap s) addr)
showInstructions :: GmCode -> Iseq
showInstructions is
	= iConcat [iStr "  Code:{",
           iIndent (iInterleave iNewline (map showInstruction is)),
           iStr "}", iNewline]
showInstruction :: Instruction -> Iseq
showInstruction Unwind         = iStr  "Unwind"
showInstruction (Pushglobal f) = (iStr "Pushglobal ") `iAppend` (iStr f)
showInstruction (Push n)       = (iStr "Push ")       `iAppend` (iNum n)
showInstruction (Pushint n)    = (iStr "Pushint ")    `iAppend` (iNum n)
showInstruction Mkap           = iStr  "Mkap"
showInstruction (Slide n)      = (iStr "Slide ")      `iAppend` (iNum n)
showStack :: GmState -> Iseq
showStack s
	= iConcat [iStr " Stack:[",
           iIndent (iInterleave iNewline
                       (map (showStackItem s) (reverse (getStack s)))),
           iStr "]"]
showStackItem :: GmState -> Addr -> Iseq
showStackItem s a
	= iConcat [iStr (showaddr a), iStr ": ",
           showNode s a (hLookup (getHeap s) a)]
showNode :: GmState -> Addr -> Node -> Iseq
showNode s a (NNum n)      = iNum n
showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
	where v = head [n | (n,b) <- getGlobals s, a==b]
showNode s a (NAp a1 a2)   = iConcat [iStr "Ap ", iStr (showaddr a1),
                                      iStr " ",   iStr (showaddr a2)]
showStats :: GmState -> Iseq
showStats s
	= iConcat [ iStr "Steps taken = ", iNum (statGetSteps (getStats s))]
data Instruction = Unwind
                | Pushglobal Name
                | Pushint Int
                | Push Int
                | Mkap
                | Update Int
                | Pop Int
instance Eq Instruction 
	where
	Unwind		== Unwind 		= True
	Pushglobal a	== Pushglobal b		= a == b
	Pushint a	== Pushint b		= a == b
	Push a 		== Push b		= a == b
	Mkap		== Mkap			= True
	Update a 	== Update b		= a == b
	_		== _			= False
rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as
	= take n as' ++ drop n as
  	where as' = map (getArg . hLookup heap) (tl as)
compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env
	= zip (map first defs) [n-1, n-2 .. 0] ++ argOffset n env
		where n = length defs
boxInteger :: Int -> GmState -> GmState
boxInteger n state
	= putStack (a: getStack state) (putHeap h' state)
  	where (h', a) = hAlloc (getHeap state) (NNum n)
unboxInteger :: Addr -> GmState -> Int
unboxInteger a state
	= ub (hLookup (getHeap state) a)
  	where 	ub (NNum i) = i
		ub n        = error "Unboxing a non-integer"
type GmState =
	(GmOutput,		-- Current Output
	 GmCode,		-- Current Instruction Stream
	 GmStack,		-- Current Stack
	 GmDump,		-- The Dump
	 GmHeap,		-- Heap of Nodes
	 GmGlobals,		-- Global addresses in Heap
	 GmStats)		-- Statistics
getOutput :: GmState -> GmOutput
getOutput (o, i, stack, dump, heap, globals, stats) = o
putOutput :: GmOutput -> GmState -> GmState
putOutput o' (o, i, stack, dump, heap, globals, stats)
	= (o', i, stack, dump, heap, globals, stats)
showState :: GmState -> Iseq
showState s
	= iConcat [showOutput s,                 iNewline,
           	showStack s,                  iNewline,
           	showDump s,                   iNewline,
           	showInstructions (getCode s), iNewline]
data Instruction 
	= Slide Int
	| Alloc Int
	| Update Int
	| Pop Int
	| Unwind
	| Pushglobal Name
	| Pushint Int
	| Push Int
	| Mkap
	| Eval
	| Add | Sub | Mul | Div
	| Neg
	| Eq | Ne | Lt | Le | Gt | Ge
	| Cond GmCode GmCode
	| Pack Int Int
	| Casejump [(Int, GmCode)]
	| Split Int
	| Print
type GmState = (GmOutput,      -- Current output
            GmCode,            -- Current instruction stream
            GmStack,           -- Current stack
            GmDump,            -- Current dump
            GmVStack,          -- Current V-stack
            GmHeap,            -- Heap of nodes
            GmGlobals,         -- Global addresses in heap
            GmStats)           -- Statistics
type GmVStack = [Int]
getVStack :: GmState -> GmVStack
getVStack (o, i, stack, dump, vstack, heap, globals, stats) = vstack
putVStack :: GmVStack -> GmState -> GmState
putVStack vstack' (o, i, stack, dump, vstack, heap, globals, stats)
	= (o, i, stack, dump, vstack', heap, globals, stats)
showState :: GmState -> Iseq
showState s
	= iConcat [showOutput s,                 iNewline,
           showStack s,                  iNewline,
           showDump s,                   iNewline,
           showVStack s,                 iNewline,
           showInstructions (getCode s), iNewline]
showVStack :: GmState -> Iseq
showVStack s
	= iConcat [iStr "Vstack:[",
           iInterleave (iStr ", ") (map iNum (getVStack s)),
           iStr "]"]
compile :: CoreProgram -> GmState
compile program
	= ([], initialCode, [], [], [], heap, globals, statInitial)
  	where (heap, globals) = buildInitialHeap program
buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program
	= mapAccuml allocateSc hInitial compiled
  	where compiled = map compileSc (preludeDefs ++ program ++ primitives)
primitives :: [(Name,[Name],CoreExpr)]
primitives
	= 	[("+", ["x","y"], (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))),
   		 ("-", ["x","y"], (EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))),
   		 ("*", ["x","y"], (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))),
   		 ("/", ["x","y"], (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))),
   		 ("negate", ["x"], (EAp (EVar "negate") (EVar "x"))),
   		 ("==", ["x","y"], (EAp (EAp (EVar "==") (EVar "x")) (EVar "y"))),
   		 ("~=", ["x","y"], (EAp (EAp (EVar "~=") (EVar "x")) (EVar "y"))),
   		 (">=", ["x","y"], (EAp (EAp (EVar ">=") (EVar "x")) (EVar "y"))),
   		 (">",  ["x","y"], (EAp (EAp (EVar ">")  (EVar "x")) (EVar "y"))),
   		 ("<=", ["x","y"], (EAp (EAp (EVar "<=") (EVar "x")) (EVar "y"))),
   		 ("<",  ["x","y"], (EAp (EAp (EVar "<")  (EVar "x")) (EVar "y"))),
   		 ("if",  ["c","t","f"],
      		(EAp (EAp (EAp (EVar "if") (EVar "c")) (EVar "t")) (EVar "f"))),
   		 ("True",  [], (EConstr 2 0)),
   		 ("False", [], (EConstr 1 0))]
