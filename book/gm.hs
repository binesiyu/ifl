module GM where
import Language
import Utils
-- The function run is already defined in gofers standard.prelude
runProg :: [Char] -> [Char]
runProg = showResults . eval . compile . parse
-- :a language.lhs -- parser data types
-- :a util.lhs -- heap data type and other library functions
{-exs_1-3-}type GmState 
{-exs_1-3-}	= (GmCode,	-- Current instruction stream
{-exs_1-3-}	GmStack,	-- Current stack
{-exs_1-3-}	GmHeap,		-- Heap of nodes
{-exs_1-3-}	GmGlobals,	-- Global addresses in heap
{-exs_1-3-}	GmStats)	-- Statistics
type GmCode = [Instruction]
getCode :: GmState -> GmCode
{-exs_1-3-}getCode (i, stack, heap, globals, stats) = i
putCode :: GmCode -> GmState -> GmState
{-exs_1-3-}putCode i' (i, stack, heap, globals, stats)
{-exs_1-3-}	= (i', stack, heap, globals, stats)
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
{-exs_1-3-}getStack (i, stack, heap, globals, stats) = stack
putStack :: GmStack -> GmState -> GmState
{-exs_1-3-}putStack stack' (i, stack, heap, globals, stats)
{-exs_1-3-}	= (i, stack', heap, globals, stats)
type GmHeap = Heap Node
getHeap :: GmState -> GmHeap
{-exs_1-3-}getHeap (i, stack, heap, globals, stats) = heap
putHeap :: GmHeap -> GmState -> GmState
{-exs_1-3-}putHeap heap' (i, stack, heap, globals, stats)
{-exs_1-3-}	= (i, stack, heap', globals, stats)
{-exs_1-}data Node 
{-exs_1-}	= NNum Int		-- Numbers
{-exs_1-}	| NAp Addr Addr		-- Applications
{-exs_1-}	| NGlobal Int GmCode 	-- Globals
type GmGlobals = ASSOC Name Addr
getGlobals :: GmState -> GmGlobals
{-exs_1-3-}getGlobals (i, stack, heap, globals, stats) = globals
statInitial  :: GmStats
statIncSteps :: GmStats -> GmStats
statGetSteps :: GmStats -> Int
type GmStats = Int
statInitial    = 0
statIncSteps s = s+1
statGetSteps s = s
getStats :: GmState -> GmStats
{-exs_1-3-}getStats (i, stack, heap, globals, stats) = stats
putStats :: GmStats -> GmState -> GmState
{-exs_1-3-}putStats stats' (i, stack, heap, globals, stats)
{-exs_1-3-}	= (i, stack, heap, globals, stats')
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
{-exs_1-6-}step state = dispatch i (putCode is state)
{-exs_1-6-}             where (i:is) = getCode state
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
{-exs_1-2-}push :: Int -> GmState -> GmState
{-exs_1-2-}push n state
{-exs_1-2-}	= putStack (a:as) state
{-exs_1-2-}  	where 	as = getStack state
{-exs_1-2-}        	a  = getArg (hLookup (getHeap state) (as !! (n+1)))
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
{-exs_1-3-}compile :: CoreProgram -> GmState
{-exs_1-3-}compile program
{-exs_1-3-}	= (initialCode, [], heap, globals, statInitial)
{-exs_1-3-}  	where (heap, globals) = buildInitialHeap program
{-exs_1-6-}buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
{-exs_1-6-}buildInitialHeap program
{-exs_1-6-}	= mapAccuml allocateSc hInitial compiled
{-exs_1-6-}  	--where compiled = map compileSc (preludeDefs ++ program) ++
{-exs_1-6-}   --                compiledPrimitives
{-exs_1-6-}  	where compiled = map compileSc program
type GmCompiledSC = (Name, Int, GmCode)
allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns)
	= (heap', (name, addr))
  	where (heap', addr) = hAlloc heap (NGlobal nargs instns)
{-exs_1-3-}initialCode :: GmCode
{-exs_1-3-}initialCode = [Pushglobal "main", Unwind]
compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body)
	= (name, length env, compileR body (zip2 env [0..]))
{-exs_1-4-}compileR :: GmCompiler
{-exs_1-}compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]
{-exs_6-}compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]
type GmCompiler = CoreExpr -> GmEnvironment -> GmCode
type GmEnvironment = ASSOC Name Int
{-exs_1-2-}compileC :: GmCompiler
{-exs_1-2-}compileC (EVar v)    env 
{-exs_1-2-}	| elem v (aDomain env)		= [Push n]
{-exs_1-2-}	| otherwise			= [Pushglobal v]
{-exs_1-2-}	where n = aLookup env v (error "Can't happen")
{-exs_1-2-}compileC (ENum n)    env = [Pushint n]
{-exs_1-2-}compileC (EAp e1 e2) env = compileC e2 env ++
{-exs_1-2-}                           compileC e1 (argOffset 1 env) ++
{-exs_1-2-}                           [Mkap]
argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v,m) <- env]
{-exs_1-3-}compiledPrimitives :: [GmCompiledSC]
{-exs_1-3-}compiledPrimitives = []
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
{-exs_1-3-}showState :: GmState -> Iseq
{-exs_1-3-}showState s
{-exs_1-3-}	= iConcat [showStack s,		iNewline,
{-exs_1-3-}	showInstructions (getCode s), 	iNewline]
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
{-exs_2-5-}data Node 
{-exs_2-5-}	= NNum Int		-- Numbers
{-exs_2-5-}	| NAp Addr Addr		-- Applications
{-exs_2-5-}	| NGlobal Int GmCode 	-- Globals
{-exs_2-5-}	| NInd Addr		-- Indirections
{-exs_2-5-}instance Eq Node
{-exs_2-5-}  where
{-exs_2-5-}  NNum a       == NNum b          = a == b    -- needed to check conditions
{-exs_2-5-}  NAp a b      == NAp c d         = False     -- not needed
{-exs_2-5-}  NGlobal a b  == NGlobal c d     = False     -- not needed
{-exs_2-5-}  NInd a       == NInd b          = False     -- not needed
rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as
	= take n as' ++ drop n as
  	where as' = map (getArg . hLookup heap) (tl as)
{-exs_3--}allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
{-exs_3--}allocNodes 0     heap = (heap,  [])
{-exs_3--}allocNodes (n+1) heap = (heap2, a:as)
{-exs_3--}                        where (heap1, as) = allocNodes n heap
{-exs_3--}                              (heap2, a)  = hAlloc heap1 (NInd hNull)
{-exs_3-5-}compileC :: GmCompiler
{-exs_3-5-}compileC (EVar v)      args 
{-exs_3-5-}	| elem v (aDomain args) = [Push n]
{-exs_3-5-}	| otherwise		= [Pushglobal v]
{-exs_3-5-}      	where n = aLookup args v (error "")
{-exs_3-5-}compileC (ENum n)    env = [Pushint n]
{-exs_3-5-}compileC (EAp e1 e2) env = compileC e2 env ++
{-exs_3-5-}                           compileC e1 (argOffset 1 env) ++
{-exs_3-5-}                           [Mkap]
{-exs_3-5-}compileC (ELet recursive defs e) args 
{-exs_3-5-}		| recursive	= compileLetrec compileC defs e args
{-exs_3-5-}		| otherwise	= compileLet    compileC defs e args
{-exs_3--}compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
{-exs_3--}compileLet comp defs expr env
{-exs_3--}	= compileLet' defs env ++ comp expr env' ++ [Slide (length defs)]
{-exs_3--}  	where env' = compileArgs defs env
{-exs_3--}compileLet' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
{-exs_3--}compileLet' []                  env = []
{-exs_3--}compileLet' ((name, expr):defs) env
{-exs_3--}	= compileC expr env ++ compileLet' defs (argOffset 1 env)
compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env
	= zip (map first defs) [n-1, n-2 .. 0] ++ argOffset n env
		where n = length defs
{-exs_4-5-}type GmState = ( GmCode, 	     -- current Instruction
{-exs_4-5-}		       GmStack,	     -- current Stack
{-exs_4-5-}		       GmDump,	     -- current Dump
{-exs_4-5-}		       GmHeap, 	     -- Heap of Nodes
{-exs_4-5-}		       GmGlobals,    -- Global adresses in Heap
{-exs_4-5-}		       GmStats)	     -- Statistics
{-exs_4--}type GmDump = [GmDumpItem]
{-exs_4--}type GmDumpItem = (GmCode, GmStack)
{-exs_4--}getDump :: GmState -> GmDump
{-exs_4-5-}getDump (i, stack, dump, heap, globals, stats) = dump
{-exs_4--}putDump :: GmDump -> GmState -> GmState
{-exs_4-5-}putDump dump' (i, stack, dump, heap, globals, stats)
{-exs_4-5-}	= (i, stack, dump', heap, globals, stats)
{-exs_4-5-}data Instruction 
{-exs_4-5-}		= Slide Int
{-exs_4-5-}		| Alloc Int
{-exs_4-5-} 		| Update Int
{-exs_4-5-}		| Pop Int
{-exs_4-5-}		| Unwind
{-exs_4-5-}		| Pushglobal Name
{-exs_4-5-}		| Pushint Int
{-exs_4-5-}		| Push Int
{-exs_4-5-}		| Mkap
{-exs_4-5-}		| Eval
{-exs_4-5-}		| Add | Sub | Mul | Div | Neg
{-exs_4-5-}		| Eq | Ne | Lt | Le | Gt | Ge
{-exs_4-5-}		| Cond GmCode GmCode
{-exs_4-5-}showState :: GmState -> Iseq
{-exs_4-5-}showState s
{-exs_4-5-}	= iConcat 	[showStack s,                  iNewline,
{-exs_4-5-}			 showDump s,                   iNewline,
{-exs_4-5-}			 showInstructions (getCode s), iNewline]
{-exs_4--}showDump :: GmState -> Iseq
{-exs_4--}showDump s
{-exs_4--}	= iConcat 	[iStr "  Dump:[",
{-exs_4--}			 iIndent (iInterleave iNewline
{-exs_4--}				(map showDumpItem (reverse (getDump s)))),
{-exs_4--}			 iStr "]"]
{-exs_4--}showDumpItem :: GmDumpItem -> Iseq
{-exs_4--}showDumpItem (code, stack)
{-exs_4--}	= iConcat	[iStr "<",
{-exs_4--}			 shortShowInstructions 3 code, iStr ", ",
{-exs_4--}			 shortShowStack stack,         iStr ">"]
{-exs_4--}shortShowInstructions :: Int -> GmCode -> Iseq
{-exs_4--}shortShowInstructions number code
{-exs_4--}	= iConcat [iStr "{", iInterleave (iStr "; ") dotcodes, iStr "}"]
{-exs_4--}  	where 	codes 	= map showInstruction (take number code)
{-exs_4--}            dotcodes	| length code > number	= codes ++ [iStr "..."]
{-exs_4--}				| otherwise		= codes
{-exs_4--}shortShowStack :: GmStack -> Iseq
{-exs_4--}shortShowStack stack
{-exs_4--}	= iConcat [iStr "[",
{-exs_4--}           iInterleave (iStr ", ") (map (iStr . showaddr) stack),
{-exs_4--}           iStr "]"]
boxInteger :: Int -> GmState -> GmState
boxInteger n state
	= putStack (a: getStack state) (putHeap h' state)
  	where (h', a) = hAlloc (getHeap state) (NNum n)
unboxInteger :: Addr -> GmState -> Int
unboxInteger a state
	= ub (hLookup (getHeap state) a)
  	where 	ub (NNum i) = i
		ub n        = error "Unboxing a non-integer"
{-exs_4-6-}primitive1 :: (b -> GmState -> GmState)	-- boxing function
{-exs_4-6-}		-> (Addr -> GmState -> a)	-- unbixing function
{-exs_4-6-}		-> (a -> b)			-- operator
{-exs_4-6-}		-> (GmState -> GmState)		-- state transition
{-exs_4-6-}primitive1 box unbox op state
{-exs_4-6-}	= box (op (unbox a state)) (putStack as state)
{-exs_4-6-}  	where (a:as) = getStack state
{-exs_4-6-}primitive2 :: (b -> GmState -> GmState)	-- boxing function
{-exs_4-6-}		-> (Addr -> GmState -> a)	-- unbixing function
{-exs_4-6-}		-> (a -> a -> b)		-- operator
{-exs_4-6-}		-> (GmState -> GmState)		-- state transition
{-exs_4-6-}primitive2 box unbox op state
{-exs_4-6-}	= box (op (unbox a0 state) (unbox a1 state)) (putStack as state)
{-exs_4-6-}  	where (a0:a1:as) = getStack state
{-exs_4-6-}arithmetic1 :: 	(Int -> Int)		-- arithmetic operator
{-exs_4-6-}			-> (GmState -> GmState)	-- state transition
{-exs_4-6-}arithmetic1 = primitive1 boxInteger unboxInteger
{-exs_4-6-}arithmetic2 ::	(Int -> Int -> Int)	-- arithmetic operation
{-exs_4-6-}			-> (GmState -> GmState)	-- state transition
{-exs_4-6-}arithmetic2 = primitive2 boxInteger unboxInteger
{-exs_4-5-}boxBoolean :: Bool -> GmState -> GmState
{-exs_4-5-}boxBoolean b state
{-exs_4-5-}	= putStack (a: getStack state) (putHeap h' state)
{-exs_4-5-}  	where 	(h',a) = hAlloc (getHeap state) (NNum b')
{-exs_4-5-}		  b' | b		= 1
{-exs_4-5-}		     | otherwise	= 0
{-exs_4-6-}comparison :: (Int -> Int -> Bool) -> GmState -> GmState
{-exs_4-6-}comparison = primitive2 boxBoolean unboxInteger
{-exs_4-5-}compile :: CoreProgram -> GmState
{-exs_4-5-}compile program
{-exs_4-5-}	= (initialCode, [], [], heap, globals, statInitial)
{-exs_4-5-}  	where (heap, globals) = buildInitialHeap program
{-exs_4-5-}initialCode :: GmCode
{-exs_4-5-}initialCode = [Pushglobal "main", Eval]
{-exs_4-6-}compiledPrimitives :: [GmCompiledSC]
{-exs_4-6-}compiledPrimitives
{-exs_4-6-}	= 	[("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind]),
{-exs_4-6-}   	 ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind]),
{-exs_4-6-}   	 ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind]),
{-exs_4-6-}   	 ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind]),
{-exs_4-6-}   	 ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind]),
{-exs_4-6-}   	 ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind]),
{-exs_4-6-}   	 ("~=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind]),
{-exs_4-6-}   	 ("<",  2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind]),
{-exs_4-6-}   	 ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind]),
{-exs_4-6-}   	 (">",  2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind]),
{-exs_4-6-}   	 (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind]),
{-exs_4-6-}   	 ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2],
{-exs_4-6-}                            Update 3, Pop 3, Unwind])]
{-exs_5--}builtInDyadic :: ASSOC Name Instruction
{-exs_5--}builtInDyadic
{-exs_5--}	= 	[("+", Add), ("-", Sub), ("*", Mul), ("div", Div),
{-exs_5--}   		 ("==", Eq), ("~=", Ne), (">=", Ge),
{-exs_5--}   		 (">",  Gt), ("<=", Le), ("<",  Lt)]
{-exs_6-}type GmState =
{-exs_6-}	(GmOutput,		-- Current Output
{-exs_6-}	 GmCode,		-- Current Instruction Stream
{-exs_6-}	 GmStack,		-- Current Stack
{-exs_6-}	 GmDump,		-- The Dump
{-exs_6-}	 GmHeap,		-- Heap of Nodes
{-exs_6-}	 GmGlobals,		-- Global addresses in Heap
{-exs_6-}	 GmStats)		-- Statistics
{-exs_6--}type GmOutput = [Char]
{-exs_6-}getOutput :: GmState -> GmOutput
{-exs_6-}getOutput (o, i, stack, dump, heap, globals, stats) = o
{-exs_6-}putOutput :: GmOutput -> GmState -> GmState
{-exs_6-}putOutput o' (o, i, stack, dump, heap, globals, stats)
{-exs_6-}	= (o', i, stack, dump, heap, globals, stats)
{-exs_6--}data Node
{-exs_6--}		= NNum Int              -- Numbers
{-exs_6--}		| NAp Addr Addr         -- Applications
{-exs_6--}		| NGlobal Int GmCode    -- Globals
{-exs_6--}		| NInd Addr
{-exs_6--}		| NConstr Int [Addr]
{-exs_6--}instance Eq Node
{-exs_6--}  where
{-exs_6--}  NNum a       == NNum b          = a == b    -- needed to check conditions
{-exs_6--}  NAp a b      == NAp c d         = False     -- not needed
{-exs_6--}  NGlobal a b  == NGlobal c d     = False     -- not needed
{-exs_6--}  NInd a       == NInd b          = False     -- not needed
{-exs_6--}  NConstr a b  == NConstr c d     = False     -- not needed
{-exs_6-}showState :: GmState -> Iseq
{-exs_6-}showState s
{-exs_6-}	= iConcat [showOutput s,                 iNewline,
{-exs_6-}           	showStack s,                  iNewline,
{-exs_6-}           	showDump s,                   iNewline,
{-exs_6-}           	showInstructions (getCode s), iNewline]
{-exs_6--}showOutput :: GmState -> Iseq
{-exs_6--}showOutput s = iConcat [iStr "Output:\"", iStr (getOutput s), iStr "\""]
{-exs_6--}showNode :: GmState -> Addr -> Node -> Iseq
{-exs_6--}showNode s a (NNum n)      = iNum n
{-exs_6--}showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
{-exs_6--}	where v = head [n | (n,b) <- getGlobals s, a==b]
{-exs_6--}showNode s a (NAp a1 a2)   = iConcat [iStr "Ap ",  iStr (showaddr a1),
{-exs_6--}                                      iStr " ",    iStr (showaddr a2)]
{-exs_6--}showNode s a (NInd a1)     = iConcat [iStr "Ind ", iStr (showaddr a1)]
{-exs_6--}showNode s a (NConstr t as)
{-exs_6--} = iConcat [iStr "Cons ", iNum t, iStr " [",
{-exs_6--}            iInterleave (iStr ", ") (map (iStr.showaddr) as), iStr "]"]
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
{-exs_6--}compileAlts ::	(Int -> GmCompiler)	-- compiler for alternative bodies
{-exs_6--}			-> [CoreAlt]		-- the list of alternatives
{-exs_6--}			-> GmEnvironment	-- the current environment
{-exs_6--}			-> [(Int, GmCode)]	-- list of alternative code sequences
{-exs_6--}compileAlts comp alts env
{-exs_6--} = [(tag, comp (length names) body (zip names [0..] ++ argOffset (length names) env))
{-exs_6--}         | (tag, names, body) <- alts]
{-exs_6--}compileE' :: Int -> GmCompiler
{-exs_6--}compileE' offset expr env
{-exs_6--}	= [Split offset] ++ compileE expr env ++ [Slide offset]
{-exs_6-7-}boxBoolean :: Bool -> GmState -> GmState
{-exs_6-7-}boxBoolean b state
{-exs_6-7-} = putStack (a: getStack state) (putHeap h' state)
{-exs_6-7-}   where (h',a) = hAlloc (getHeap state) (NConstr b' [])
{-exs_6-7-}         b' | b = 2		-- 2 is tag of True
{-exs_6-7-}            | otherwise = 1	-- 1 is tag of False
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
