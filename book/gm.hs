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
{-exs_1-}data Instruction 
{-exs_1-}    = Unwind
{-exs_1-}    | Pushglobal Name
{-exs_1-}    | Pushint Int
{-exs_1-}    | Push Int
{-exs_1-}    | Mkap
{-exs_1-}    | Slide Int
{-exs_1-}instance Eq Instruction 
{-exs_1-}    where
{-exs_1-}    Unwind          == Unwind               = True
{-exs_1-}    Pushglobal a    == Pushglobal b         = a == b
{-exs_1-}    Pushint a       == Pushint b            = a == b
{-exs_1-}    Push a          == Push b               = a == b
{-exs_1-}    Mkap            == Mkap                 = True
{-exs_1-}    Slide a         == Slide b              = a == b
{-exs_1-}    _               == _                    = False
type GmStack = [Addr]
getStack :: GmState -> GmStack
putStack :: GmStack -> GmState -> GmState
type GmHeap = Heap Node
getHeap :: GmState -> GmHeap
putHeap :: GmHeap -> GmState -> GmState
{-exs_1-}data Node 
{-exs_1-}	= NNum Int		-- Numbers
{-exs_1-}	| NAp Addr Addr		-- Applications
{-exs_1-}	| NGlobal Int GmCode 	-- Globals
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
{-exs_1-}dispatch :: Instruction -> GmState -> GmState
{-exs_1-}dispatch (Pushglobal f) = pushglobal f
{-exs_1-}dispatch (Pushint n)    = pushint n
{-exs_1-}dispatch Mkap           = mkap
{-exs_1-}dispatch (Push n)       = push n
{-exs_1-}dispatch (Slide n)      = slide n
{-exs_1-}dispatch Unwind         = unwind
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
{-exs_1-}unwind :: GmState -> GmState
{-exs_1-}unwind state
{-exs_1-}	= newState (hLookup heap a)
{-exs_1-}  	where
{-exs_1-}  		(a:as) = getStack state
{-exs_1-}  		heap   = getHeap state
{-exs_1-}  		newState (NNum n)      = state
{-exs_1-}  		newState (NAp a1 a2)   = putCode [Unwind] (putStack (a1:a:as) state)
{-exs_1-} 	          newState (NGlobal n c) 
{-exs_1-} 		         | length as < n	= error "Unwinding with too few arguments"
{-exs_1-}		         | otherwise	= putCode c state
type GmCompiledSC = (Name, Int, GmCode)
allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns)
	= (heap', (name, addr))
  	where (heap', addr) = hAlloc heap (NGlobal nargs instns)
compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body)
	= (name, length env, compileR body (zip2 env [0..]))
{-exs_1-}compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]
{-exs_6-}compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]
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
{-exs_1-}showInstruction :: Instruction -> Iseq
{-exs_1-}showInstruction Unwind         = iStr  "Unwind"
{-exs_1-}showInstruction (Pushglobal f) = (iStr "Pushglobal ") `iAppend` (iStr f)
{-exs_1-}showInstruction (Push n)       = (iStr "Push ")       `iAppend` (iNum n)
{-exs_1-}showInstruction (Pushint n)    = (iStr "Pushint ")    `iAppend` (iNum n)
{-exs_1-}showInstruction Mkap           = iStr  "Mkap"
{-exs_1-}showInstruction (Slide n)      = (iStr "Slide ")      `iAppend` (iNum n)
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
{-exs_1-}showNode :: GmState -> Addr -> Node -> Iseq
{-exs_1-}showNode s a (NNum n)      = iNum n
{-exs_1-}showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
{-exs_1-}	where v = head [n | (n,b) <- getGlobals s, a==b]
{-exs_1-}showNode s a (NAp a1 a2)   = iConcat [iStr "Ap ", iStr (showaddr a1),
{-exs_1-}                                      iStr " ",   iStr (showaddr a2)]
showStats :: GmState -> Iseq
showStats s
	= iConcat [ iStr "Steps taken = ", iNum (statGetSteps (getStats s))]
{-exs_2-}data Instruction = Unwind
{-exs_2-}                | Pushglobal Name
{-exs_2-}                | Pushint Int
{-exs_2-}                | Push Int
{-exs_2-}                | Mkap
{-exs_2-}                | Update Int
{-exs_2-}                | Pop Int
{-exs_2-}instance Eq Instruction 
{-exs_2-}	where
{-exs_2-}	Unwind		== Unwind 		= True
{-exs_2-}	Pushglobal a	== Pushglobal b		= a == b
{-exs_2-}	Pushint a	== Pushint b		= a == b
{-exs_2-}	Push a 		== Push b		= a == b
{-exs_2-}	Mkap		== Mkap			= True
{-exs_2-}	Update a 	== Update b		= a == b
{-exs_2-}	_		== _			= False
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
{-exs_6-}type GmState =
{-exs_6-}	(GmOutput,		-- Current Output
{-exs_6-}	 GmCode,		-- Current Instruction Stream
{-exs_6-}	 GmStack,		-- Current Stack
{-exs_6-}	 GmDump,		-- The Dump
{-exs_6-}	 GmHeap,		-- Heap of Nodes
{-exs_6-}	 GmGlobals,		-- Global addresses in Heap
{-exs_6-}	 GmStats)		-- Statistics
{-exs_6-}getOutput :: GmState -> GmOutput
{-exs_6-}getOutput (o, i, stack, dump, heap, globals, stats) = o
{-exs_6-}putOutput :: GmOutput -> GmState -> GmState
{-exs_6-}putOutput o' (o, i, stack, dump, heap, globals, stats)
{-exs_6-}	= (o', i, stack, dump, heap, globals, stats)
{-exs_6-}showState :: GmState -> Iseq
{-exs_6-}showState s
{-exs_6-}	= iConcat [showOutput s,                 iNewline,
{-exs_6-}           	showStack s,                  iNewline,
{-exs_6-}           	showDump s,                   iNewline,
{-exs_6-}           	showInstructions (getCode s), iNewline]
{-exs_6-}data Instruction 
{-exs_6-}	= Slide Int
{-exs_6-}	| Alloc Int
{-exs_6-}	| Update Int
{-exs_6-}	| Pop Int
{-exs_6-}	| Unwind
{-exs_6-}	| Pushglobal Name
{-exs_6-}	| Pushint Int
{-exs_6-}	| Push Int
{-exs_6-}	| Mkap
{-exs_6-}	| Eval
{-exs_6-}	| Add | Sub | Mul | Div
{-exs_6-}	| Neg
{-exs_6-}	| Eq | Ne | Lt | Le | Gt | Ge
{-exs_6-}	| Cond GmCode GmCode
{-exs_6-}	| Pack Int Int
{-exs_6-}	| Casejump [(Int, GmCode)]
{-exs_6-}	| Split Int
{-exs_6-}	| Print
{-exs_7-}type GmState = (GmOutput,      -- Current output
{-exs_7-}            GmCode,            -- Current instruction stream
{-exs_7-}            GmStack,           -- Current stack
{-exs_7-}            GmDump,            -- Current dump
{-exs_7-}            GmVStack,          -- Current V-stack
{-exs_7-}            GmHeap,            -- Heap of nodes
{-exs_7-}            GmGlobals,         -- Global addresses in heap
{-exs_7-}            GmStats)           -- Statistics
{-exs_7-}type GmVStack = [Int]
{-exs_7-}getVStack :: GmState -> GmVStack
{-exs_7-}getVStack (o, i, stack, dump, vstack, heap, globals, stats) = vstack
{-exs_7-}putVStack :: GmVStack -> GmState -> GmState
{-exs_7-}putVStack vstack' (o, i, stack, dump, vstack, heap, globals, stats)
{-exs_7-}	= (o, i, stack, dump, vstack', heap, globals, stats)
{-exs_7-}showState :: GmState -> Iseq
{-exs_7-}showState s
{-exs_7-}	= iConcat [showOutput s,                 iNewline,
{-exs_7-}           showStack s,                  iNewline,
{-exs_7-}           showDump s,                   iNewline,
{-exs_7-}           showVStack s,                 iNewline,
{-exs_7-}           showInstructions (getCode s), iNewline]
{-exs_7-}showVStack :: GmState -> Iseq
{-exs_7-}showVStack s
{-exs_7-}	= iConcat [iStr "Vstack:[",
{-exs_7-}           iInterleave (iStr ", ") (map iNum (getVStack s)),
{-exs_7-}           iStr "]"]
{-exs_7-}compile :: CoreProgram -> GmState
{-exs_7-}compile program
{-exs_7-}	= ([], initialCode, [], [], [], heap, globals, statInitial)
{-exs_7-}  	where (heap, globals) = buildInitialHeap program
{-exs_7-}buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
{-exs_7-}buildInitialHeap program
{-exs_7-}	= mapAccuml allocateSc hInitial compiled
{-exs_7-}  	where compiled = map compileSc (preludeDefs ++ program ++ primitives)
{-exs_7-}primitives :: [(Name,[Name],CoreExpr)]
{-exs_7-}primitives
{-exs_7-}	= 	[("+", ["x","y"], (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))),
{-exs_7-}   		 ("-", ["x","y"], (EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))),
{-exs_7-}   		 ("*", ["x","y"], (EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))),
{-exs_7-}   		 ("/", ["x","y"], (EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))),
{-exs_7-}   		 ("negate", ["x"], (EAp (EVar "negate") (EVar "x"))),
{-exs_7-}   		 ("==", ["x","y"], (EAp (EAp (EVar "==") (EVar "x")) (EVar "y"))),
{-exs_7-}   		 ("~=", ["x","y"], (EAp (EAp (EVar "~=") (EVar "x")) (EVar "y"))),
{-exs_7-}   		 (">=", ["x","y"], (EAp (EAp (EVar ">=") (EVar "x")) (EVar "y"))),
{-exs_7-}   		 (">",  ["x","y"], (EAp (EAp (EVar ">")  (EVar "x")) (EVar "y"))),
{-exs_7-}   		 ("<=", ["x","y"], (EAp (EAp (EVar "<=") (EVar "x")) (EVar "y"))),
{-exs_7-}   		 ("<",  ["x","y"], (EAp (EAp (EVar "<")  (EVar "x")) (EVar "y"))),
{-exs_7-}   		 ("if",  ["c","t","f"],
{-exs_7-}      		(EAp (EAp (EAp (EVar "if") (EVar "c")) (EVar "t")) (EVar "f"))),
{-exs_7-}   		 ("True",  [], (EConstr 2 0)),
{-exs_7-}   		 ("False", [], (EConstr 1 0))]
