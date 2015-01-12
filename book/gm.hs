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
type GmStack = [Addr]
getStack :: GmState -> GmStack
putStack :: GmStack -> GmState -> GmState
type GmHeap = Heap Node
getHeap :: GmState -> GmHeap
putHeap :: GmHeap -> GmState -> GmState
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
type GmCompiledSC = (Name, Int, GmCode)
allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns)
	= (heap', (name, addr))
  	where (heap', addr) = hAlloc heap (NGlobal nargs instns)
compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body)
	= (name, length env, compileR body (zip2 env [0..]))
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
showStats :: GmState -> Iseq
showStats s
	= iConcat [ iStr "Steps taken = ", iNum (statGetSteps (getStats s))]
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
