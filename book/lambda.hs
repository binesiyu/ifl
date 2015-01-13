-- :a utils.lhs
-- :a language.lhs
module Lambda where
import Utils
import Language
type AnnExpr a b = (b, AnnExpr' a b)
data AnnExpr' a b = AVar Name
                  | ANum Int
                  | AConstr Int Int
                  | AAp (AnnExpr a b) (AnnExpr a b)
                  | ALet Bool [AnnDefn a b] (AnnExpr a b)
                  | ACase (AnnExpr a b) [AnnAlt a b]
                  | ALam [a] (AnnExpr a b)
type AnnDefn a b = (a, AnnExpr a b)
type AnnAlt a b  = (Int, [a], (AnnExpr a b))
type AnnProgram a b = [(Name, [a], AnnExpr a b)]
lambdaLift :: CoreProgram -> CoreProgram
freeVars :: CoreProgram -> AnnProgram Name (Set Name)
abstract :: AnnProgram Name (Set Name) -> CoreProgram
{-exs_1-3-}rename :: CoreProgram -> CoreProgram
collectSCs :: CoreProgram -> CoreProgram
lambdaLift = collectSCs . rename . abstract . freeVars
runS = pprint . lambdaLift . parse
freeVars_e :: (Set Name)                   -- Candidates for free variables
              -> CoreExpr                  -- Expression to annotate
              -> AnnExpr Name (Set Name)   -- Annotated result
freeVars prog = [ (name, args, freeVars_e (setFromList args) body)
                | (name, args, body) <- prog
                ]
freeVars_e lv (ENum k)      = (setEmpty, ANum k)
freeVars_e lv (EVar v) | setElementOf v lv = (setSingleton v, AVar v)
                       | otherwise         = (setEmpty, AVar v)
freeVars_e lv (EAp e1 e2)
 = (setUnion (freeVarsOf e1') (freeVarsOf e2'), AAp e1' e2')
   where e1'            = freeVars_e lv e1
         e2'            = freeVars_e lv e2
freeVars_e lv (ELam args body)
 = (setSubtraction (freeVarsOf body') (setFromList args), ALam args body')
   where body'          = freeVars_e new_lv body
         new_lv         = setUnion lv (setFromList args)
freeVars_e lv (ELet is_rec defns body)
 = (setUnion defnsFree bodyFree, ALet is_rec defns' body')
   where binders        = bindersOf defns
         binderSet      = setFromList binders
         body_lv        = setUnion lv binderSet
         rhs_lv | is_rec    = body_lv
                | otherwise = lv

         rhss'          = map (freeVars_e rhs_lv) (rhssOf defns)
         defns'         = zip2 binders rhss'
         freeInValues   = setUnionList (map freeVarsOf rhss')
         defnsFree | is_rec    = setSubtraction freeInValues binderSet
                   | otherwise = freeInValues
         body'          = freeVars_e body_lv body
         bodyFree       = setSubtraction (freeVarsOf body') binderSet
freeVars_e lv (ECase e alts) = freeVars_case lv e alts
freeVarsOf :: AnnExpr Name (Set Name) -> Set Name
freeVarsOf (free_vars, expr) = free_vars
freeVarsOf_alt :: AnnAlt Name (Set Name) -> Set Name
freeVarsOf_alt (tag, args, rhs)
 = setSubtraction (freeVarsOf rhs) (setFromList args)
abstract prog = [ (sc_name, args, abstract_e rhs)
                   | (sc_name, args, rhs) <- prog
                   ]
abstract_e :: AnnExpr Name (Set Name) -> CoreExpr
abstract_e (free, AVar v)    = EVar v
abstract_e (free, ANum k)    = ENum k
abstract_e (free, AAp e1 e2) = EAp (abstract_e e1) (abstract_e e2)
abstract_e (free, ALet is_rec defns body)
 =  ELet is_rec [ (name, abstract_e body) | (name, body) <- defns]
                  (abstract_e body)
abstract_e (free, ALam args body)
 = foldll EAp sc (map EVar fvList)
   where
   fvList = setToList free
   sc = ELet nonRecursive [("sc",sc_rhs)] (EVar "sc")
   sc_rhs = ELam (fvList ++ args) (abstract_e body)
abstract_e (free, ACase e alts) = abstract_case free e alts
rename_e :: ASSOC Name Name                   -- Binds old names to new
            -> NameSupply                     -- Name supply
            -> CoreExpr                       -- Input expression
            -> (NameSupply, CoreExpr)         -- Depleted supply and result
{-exs_1-3-}rename prog
{-exs_1-3-} = second (mapAccuml rename_sc initialNameSupply prog)
{-exs_1-3-}   where
{-exs_1-3-}   rename_sc ns (sc_name, args, rhs)
{-exs_1-3-}    = (ns2, (sc_name, args', rhs'))
{-exs_1-3-}      where
{-exs_1-3-}      (ns1, args', env) = newNames ns args
{-exs_1-3-}      (ns2, rhs') = rename_e env ns1 rhs
newNames :: NameSupply -> [Name] -> (NameSupply, [Name], ASSOC Name Name)
newNames ns old_names
 = (ns', new_names, env)
   where
   (ns', new_names) = getNames ns old_names
   env = zip2 old_names new_names
rename_e env ns (EVar v)      = (ns, EVar (aLookup env v v))
rename_e env ns (ENum n)      = (ns, ENum n)
rename_e env ns (EAp e1 e2)
 = (ns2, EAp e1' e2')
   where
   (ns1, e1') = rename_e env ns e1
   (ns2, e2') = rename_e env ns1 e2
rename_e env ns (ELam args body)
 = (ns1, ELam args' body')
   where
   (ns1, args', env') = newNames ns args
   (ns2, body') = rename_e (env' ++ env) ns1 body
rename_e env ns (ELet is_rec defns body)
 = (ns3, ELet is_rec (zip2 binders' rhss') body')
   where
   (ns1, body') = rename_e body_env ns body
   binders = bindersOf defns
   (ns2, binders', env') = newNames ns1 binders
   body_env = env' ++ env
   (ns3, rhss') = mapAccuml (rename_e rhsEnv) ns2 (rhssOf defns)
   rhsEnv | is_rec    = body_env
          | otherwise = env
rename_e env ns (ECase e alts) = rename_case env ns e alts
collectSCs_e :: CoreExpr -> ([CoreScDefn], CoreExpr)
collectSCs prog
 = concat (map collect_one_sc prog)
   where
   collect_one_sc (sc_name, args, rhs)
    = (sc_name, args, rhs') : scs
     where
     (scs, rhs') = collectSCs_e rhs
collectSCs_e (ENum k)      = ([], ENum k)
collectSCs_e (EVar v)      = ([], EVar v)
collectSCs_e (EAp e1 e2)   = (scs1 ++ scs2, EAp e1' e2')
                             where
                             (scs1, e1') = collectSCs_e e1
                             (scs2, e2') = collectSCs_e e2
collectSCs_e (ELam args body) = (scs, ELam args body')
                                where
                                (scs, body') = collectSCs_e body
collectSCs_e (EConstr t a) = ([], EConstr t a)
collectSCs_e (ECase e alts)
 = (scs_e ++ scs_alts, ECase e' alts')
   where
   (scs_e, e') = collectSCs_e e
   (scs_alts, alts') = mapAccuml collectSCs_alt [] alts
   collectSCs_alt scs (tag, args, rhs) = (scs++scs_rhs, (tag, args, rhs'))
                                         where
                                         (scs_rhs, rhs') = collectSCs_e rhs
collectSCs_e (ELet is_rec defns body)
 = (rhss_scs ++ body_scs ++ local_scs, mkELet is_rec non_scs' body')
   where
   (rhss_scs,defns') = mapAccuml collectSCs_d [] defns

   scs'     = [(name,rhs) | (name,rhs) <- defns',  isELam rhs ]
   non_scs' = [(name,rhs) | (name,rhs) <- defns',  not (isELam rhs)]
   local_scs = [(name,args,body) | (name,ELam args body) <- scs']

   (body_scs, body') = collectSCs_e body

   collectSCs_d scs (name,rhs) = (scs ++ rhs_scs, (name, rhs'))
                                 where
                                 (rhs_scs, rhs') = collectSCs_e rhs
isELam :: Expr a -> Bool
isELam (ELam args body) = True
isELam other            = False
{-exs_1-}mkELet is_rec defns body = ELet is_rec defns body
{-exs_3--}abstractJ :: AnnProgram Name (Set Name) -> CoreProgram
{-exs_3--}lambdaLiftJ = collectSCs . abstractJ . freeVars . rename
{-exs_3--}runJ = pprint . lambdaLiftJ . parse
{-exs_3--}abstractJ_e :: ASSOC Name [Name]              -- Maps each new SC to
{-exs_3--}                                              --   the free vars of its group
{-exs_3--}               -> AnnExpr Name (Set Name)     -- Input expression
{-exs_3--}               -> CoreExpr                    -- Result expression
{-exs_3--}abstractJ prog = [ (name,args,abstractJ_e [] rhs)
{-exs_3--}                 | (name, args, rhs) <- prog]
{-exs_3--}abstractJ_e env (free, ANum n)      = ENum n
{-exs_3--}abstractJ_e env (free, AConstr t a) = EConstr t a
{-exs_3--}abstractJ_e env (free, AAp e1 e2)   = EAp (abstractJ_e env e1)
{-exs_3--}                                          (abstractJ_e env e2)
{-exs_3--}abstractJ_e env (free, AVar g)
{-exs_3--} = foldll EAp (EVar g) (map EVar (aLookup env g []))
{-exs_3--}abstractJ_e env (free, ALam args body)
{-exs_3--} = foldll EAp sc (map EVar fv_list)
{-exs_3--}   where
{-exs_3--}   fv_list = actualFreeList env free
{-exs_3--}   sc = ELet nonRecursive [("sc",sc_rhs)] (EVar "sc")
{-exs_3--}   sc_rhs = ELam (fv_list ++ args) (abstractJ_e env body)
{-exs_3--}abstractJ_e env (free, ALet isrec defns body)
{-exs_3--} = ELet isrec (fun_defns' ++ var_defns') body'
{-exs_3--}   where
{-exs_3--}   fun_defns = [(name,rhs) | (name,rhs) <- defns,  isALam rhs ]
{-exs_3--}   var_defns = [(name,rhs) | (name,rhs) <- defns,  not (isALam rhs)]
{-exs_3--}   fun_names = bindersOf fun_defns
{-exs_3--}   free_in_funs = setSubtraction
{-exs_3--}                       (setUnionList [freeVarsOf rhs | (name,rhs)<-fun_defns])
{-exs_3--}                       (setFromList fun_names)
{-exs_3--}   vars_to_abstract = actualFreeList env free_in_funs
{-exs_3--}   body_env = [(fun_name, vars_to_abstract) | fun_name <- fun_names] ++ env
{-exs_3--}   rhs_env | isrec     = body_env
{-exs_3--}           | otherwise = env
{-exs_3--}   fun_defns' = [ (name, ELam (vars_to_abstract ++ args)
{-exs_3--}                              (abstractJ_e rhs_env body))
{-exs_3--}                | (name, (free, ALam args body)) <- fun_defns
{-exs_3--}                ]
{-exs_3--}   var_defns' = [(name, abstractJ_e rhs_env rhs) | (name, rhs) <- var_defns]
{-exs_3--}   body' = abstractJ_e body_env body
{-exs_3--}actualFreeList :: ASSOC Name [Name] -> Set Name -> [Name]
{-exs_3--}actualFreeList env free
{-exs_3--} = setToList (setUnionList [ setFromList (aLookup env name [name])
{-exs_3--}                           | name <- setToList free
{-exs_3--}                           ])
{-exs_3--}isALam :: AnnExpr a b -> Bool
{-exs_3--}isALam (free, ALam args body) = True
{-exs_3--}isALam other                  = False
{-exs_4--}separateLams :: CoreProgram -> CoreProgram
{-exs_4--}type Level = Int
{-exs_4--}addLevels :: CoreProgram -> AnnProgram (Name, Level) Level
{-exs_4--}identifyMFEs :: AnnProgram (Name, Level) Level -> Program (Name, Level)
{-exs_4--}renameL ::  Program (Name, a) -> Program (Name, a)
{-exs_4--}float :: Program (Name,Level) -> CoreProgram
{-exs_4--}fullyLazyLift = float . renameL . identifyMFEs . addLevels . separateLams
{-exs_4--}runF          = pprint . lambdaLift . fullyLazyLift . parse
{-exs_4--}separateLams_e :: CoreExpr -> CoreExpr
{-exs_4--}separateLams_e (EVar v) = EVar v
{-exs_4--}separateLams_e (EConstr t a) = EConstr t a
{-exs_4--}separateLams_e (ENum n) = ENum n
{-exs_4--}separateLams_e (EAp e1 e2) = EAp (separateLams_e e1) (separateLams_e e2)
{-exs_4--}separateLams_e (ECase e alts)
{-exs_4--} = ECase (separateLams_e e) [ (tag, args, separateLams_e e)
{-exs_4--}                            | (tag, args, e) <- alts
{-exs_4--}                            ]
{-exs_4--}
{-exs_4--}separateLams_e (ELam args body) = mkSepLams args (separateLams_e body)
{-exs_4--}
{-exs_4--}separateLams_e (ELet is_rec defns body)
{-exs_4--} = ELet is_rec [(name, separateLams_e rhs) | (name,rhs) <- defns]
{-exs_4--}              (separateLams_e body)
{-exs_4--}mkSepLams args body = foldr mkSepLam body args
{-exs_4--}                      where mkSepLam arg body = ELam [arg] body
{-exs_4--}separateLams prog = [ (name, [], mkSepLams args (separateLams_e rhs))
{-exs_4--}                    | (name, args, rhs) <- prog
{-exs_4--}                    ]
{-exs_4--}freeSetToLevel :: ASSOC Name Level -> Set Name -> Level
{-exs_4--}freeSetToLevel env free
{-exs_4--} = foldll max 0 [aLookup env n 0 | n <- setToList free]
{-exs_4--}   -- If there are no free variables, return level zero
{-exs_4--}addLevels = freeToLevel . freeVars
{-exs_4--}freeToLevel_e :: Level                    -> -- Level of context
{-exs_4--}                 ASSOC Name Level         -> -- Level of in-scope names
{-exs_4--}                 AnnExpr Name (Set Name)  -> -- Input expression
{-exs_4--}                 AnnExpr (Name, Level) Level -- Result expression
{-exs_4--}freeToLevel prog = map freeToLevel_sc prog

{-exs_4--}freeToLevel_sc (sc_name, [], rhs) = (sc_name, [], freeToLevel_e 0 [] rhs)
{-exs_4--}freeToLevel_e level env (free, ANum k) = (0, ANum k)
{-exs_4--}freeToLevel_e level env (free, AVar v) = (aLookup env v 0, AVar v)
{-exs_4--}freeToLevel_e level env (free, AConstr t a) = (0, AConstr t a)
{-exs_4--}freeToLevel_e level env (free, AAp e1 e2)
{-exs_4--} = (max (levelOf e1') (levelOf e2'), AAp e1' e2')
{-exs_4--}    where
{-exs_4--}    e1' = freeToLevel_e level env e1
{-exs_4--}    e2' = freeToLevel_e level env e2
{-exs_4--}freeToLevel_e level env (free, ALam args body)
{-exs_4--} = (freeSetToLevel env free, ALam args' body')
{-exs_4--}   where
{-exs_4--}   body' = freeToLevel_e (level + 1) (args' ++ env) body
{-exs_4--}   args' = [(arg, level+1) | arg <- args]
{-exs_4--}freeToLevel_e level env (free, ALet is_rec defns body)
{-exs_4--} = (levelOf new_body, ALet is_rec new_defns new_body)
{-exs_4--}   where
{-exs_4--}   binders = bindersOf defns
{-exs_4--}   rhss = rhssOf defns
{-exs_4--}
{-exs_4--}   new_binders = [(name,max_rhs_level) | name <- binders]
{-exs_4--}   new_rhss = map (freeToLevel_e level rhs_env) rhss
{-exs_4--}   new_defns = zip2 new_binders new_rhss
{-exs_4--}   new_body = freeToLevel_e level body_env body
{-exs_4--}
{-exs_4--}   free_in_rhss = setUnionList [free | (free,rhs) <- rhss]
{-exs_4--}   max_rhs_level = freeSetToLevel level_rhs_env free_in_rhss
{-exs_4--}
{-exs_4--}   body_env      = new_binders ++ env
{-exs_4--}   rhs_env | is_rec           = body_env
{-exs_4--}           | otherwise        = env
{-exs_4--}   level_rhs_env |  is_rec    = [(name,0) | name <- binders] ++ env
{-exs_4--}                 |  otherwise = env
{-exs_4--}freeToLevel_e level env (free, ACase e alts)
{-exs_4--} = freeToLevel_case level env free e alts
{-exs_4--}levelOf :: AnnExpr a Level -> Level
{-exs_4--}levelOf (level, e) = level
{-exs_4--}identifyMFEs_e :: Level                               -- Level of context
{-exs_4--}                  -> AnnExpr (Name, Level) Level      -- Input expression
{-exs_4--}                  -> Expr (Name, Level)               -- Result
{-exs_4--}identifyMFEs prog = [ (sc_name, [], identifyMFEs_e 0 rhs)
{-exs_4--}                    | (sc_name, [], rhs) <- prog
{-exs_4--}                    ]
{-exs_4--}notMFECandidate (AConstr t a) = True
{-exs_4--}notMFECandidate (ANum k)      = True
{-exs_4--}notMFECandidate (AVar v)      = True
{-exs_4--}notMFECandidate ae            = False -- For now everything else
{-exs_4--}                                      --         is a candidate
{-exs_4--}identifyMFEs_e cxt (level, e)
{-exs_4--} | level == cxt || notMFECandidate e = e'
{-exs_4--} | otherwise = transformMFE level e'
{-exs_4--}   where
{-exs_4--}   e' = identifyMFEs_e1 level e
{-exs_4--}transformMFE level e = ELet nonRecursive [(("v",level), e)] (EVar "v")
{-exs_4--}identifyMFEs_e1 :: Level                              -- Level of context
{-exs_4--}                   -> AnnExpr' (Name,Level) Level     -- Input expressions
{-exs_4--}                   -> Expr (Name,Level)               -- Result expression
{-exs_4--}identifyMFEs_e1 level (AConstr t a)           = EConstr t a
{-exs_4--}identifyMFEs_e1 level (ANum n)                = ENum n
{-exs_4--}identifyMFEs_e1 level (AVar v)                = EVar v
{-exs_4--}identifyMFEs_e1 level (AAp e1 e2)
{-exs_4--} = EAp (identifyMFEs_e level e1) (identifyMFEs_e level e2)
{-exs_4--}identifyMFEs_e1 level (ALam args body)
{-exs_4--} = ELam args (identifyMFEs_e arg_level body)
{-exs_4--}   where
{-exs_4--}   (name, arg_level) = hd args
{-exs_4--}identifyMFEs_e1 level (ALet is_rec defns body)
{-exs_4--} = ELet is_rec defns' body'
{-exs_4--}   where
{-exs_4--}   body' = identifyMFEs_e level body
{-exs_4--}   defns' = [ ((name, rhs_level), identifyMFEs_e rhs_level rhs)
{-exs_4--}            | ((name, rhs_level), rhs) <- defns
{-exs_4--}            ]
{-exs_4--}identifyMFEs_e1 level (ACase e alts) = identifyMFEs_case1 level e alts
{-exs_4--}renameGen :: (NameSupply -> [a]  -> (NameSupply, [a], ASSOC Name Name))
{-exs_4--}                                      -- New-binders function
{-exs_4--}             -> Program a             -- Program to be renamed
{-exs_4--}             -> Program a             -- Resulting program
{-exs_4--}rename :: CoreProgram -> CoreProgram
{-exs_4--}rename prog = renameGen newNames prog
{-exs_4--}renameL prog = renameGen newNamesL prog
{-exs_4--}newNamesL ns old_binders
{-exs_4--} = (ns', new_binders, env)
{-exs_4--}   where
{-exs_4--}   old_names = [name | (name,level) <- old_binders]
{-exs_4--}   levels        = [level | (name,level) <- old_binders]
{-exs_4--}   (ns', new_names) = getNames ns old_names
{-exs_4--}   new_binders =  zip2 new_names levels
{-exs_4--}   env = zip2 old_names new_names
{-exs_4--}renameGen_e :: (NameSupply -> [a]  -> (NameSupply, [a], ASSOC Name Name))
{-exs_4--}                                              -- New-binders function
{-exs_4--}               -> ASSOC Name Name             -- Maps old names to new ones
{-exs_4--}               -> NameSupply                  -- Name supply
{-exs_4--}               -> Expr a                      -- Expression to be renamed
{-exs_4--}               -> (NameSupply, Expr a)        -- Depleted name supply
{-exs_4--}                                              -- and result expression
{-exs_4--}renameGen new_binders prog
{-exs_4--} = second (mapAccuml rename_sc initialNameSupply prog)
{-exs_4--}    where
{-exs_4--}    rename_sc ns (sc_name, args, rhs)
{-exs_4--}     = (ns2, (sc_name, args', rhs'))
{-exs_4--}        where
{-exs_4--}       (ns1, args', env) = new_binders ns args
{-exs_4--}       (ns2, rhs') = renameGen_e new_binders env ns1 rhs
{-exs_4--}float_e :: Expr (Name, Level) -> (FloatedDefns, Expr Name)
{-exs_4--}type FloatedDefns = [(Level, IsRec, [(Name, Expr Name)])]
{-exs_4--}float_e (EVar v) = ([], EVar v)
{-exs_4--}float_e (EConstr t a) = ([], EConstr t a)
{-exs_4--}float_e (ENum n) = ([], ENum n)
{-exs_4--}float_e (EAp e1 e2) = (fd1 ++ fd2, EAp e1' e2')
{-exs_4--}                      where
{-exs_4--}                      (fd1, e1') = float_e e1
{-exs_4--}                      (fd2, e2') = float_e e2
{-exs_4--}float_e (ELam args body)
{-exs_4--} = (fd_outer, ELam args' (install fd_this_level body'))
{-exs_4--}   where
{-exs_4--}   args' = [arg | (arg,level) <- args]
{-exs_4--}   (first_arg,this_level) = hd args
{-exs_4--}   (fd_body, body') = float_e body
{-exs_4--}   (fd_outer, fd_this_level) = partitionFloats this_level fd_body
{-exs_4--}float_e (ELet is_rec defns body)
{-exs_4--} = (rhsFloatDefns ++ [thisGroup] ++ bodyFloatDefns, body')
{-exs_4--}   where
{-exs_4--}   (bodyFloatDefns, body') = float_e body
{-exs_4--}   (rhsFloatDefns, defns') = mapAccuml float_defn [] defns
{-exs_4--}   thisGroup = (thisLevel, is_rec, defns')
{-exs_4--}   (name,thisLevel) = hd (bindersOf defns)
{-exs_4--}
{-exs_4--}   float_defn floatedDefns ((name,level), rhs)
{-exs_4--}    = (rhsFloatDefns ++ floatedDefns, (name, rhs'))
{-exs_4--}      where
{-exs_4--}      (rhsFloatDefns, rhs') = float_e rhs
{-exs_4--}float_e (ECase e alts) = float_case e alts
{-exs_4--}partitionFloats :: Level -> FloatedDefns -> (FloatedDefns, FloatedDefns)
{-exs_4--}partitionFloats this_level fds
{-exs_4--} = (filter is_outer_level fds, filter is_this_level fds)
{-exs_4--}   where
{-exs_4--}   is_this_level  (level,is_rec,defns) = level >= this_level
{-exs_4--}   is_outer_level (level,is_rec,defns) = level <  this_level
{-exs_4--}install :: FloatedDefns -> Expr Name -> Expr Name
{-exs_4--}install defnGroups e
{-exs_4--} = foldr installGroup e defnGroups
{-exs_4--}   where
{-exs_4--}   installGroup (level, is_rec, defns) e = ELet is_rec defns e
{-exs_4--}float prog = concat (map float_sc prog)
{-exs_4--}float_sc (name, [], rhs)
{-exs_4--} = [(name, [], rhs')] ++ concat (map to_scs fds)
{-exs_4--}   where
{-exs_4--}   (fds, rhs') = float_e rhs
{-exs_4--}   to_scs (level, is_rec, defns) = map make_sc defns
{-exs_4--}   make_sc (name, rhs) = (name, [], rhs)
{-exs_5--}mkELam :: [Name] -> CoreExpr -> CoreExpr
{-exs_5--}mkELam args (ELam args' body) = ELam (args++args') body
{-exs_5--}mkELam args other_body        = ELam args          other_body
{-exs_6--}depthFirstSearch :: Ord a =>
{-exs_6--}                    (a -> [a])   -> -- Map
{-exs_6--}                    (Set a, [a]) -> -- State: visited set,
{-exs_6--}                                    --        current sequence of vertices
{-exs_6--}                    [a]          -> -- Input vertices sequence
{-exs_6--}                    (Set a, [a])    -- Final state
{-exs_6--}depthFirstSearch
{-exs_6--} = foldll . search
{-exs_6--}   where
{-exs_6--}   search relation (visited, sequence) vertex
{-exs_6--}    | setElementOf vertex visited = (visited,          sequence ) -- KH
{-exs_6--}    -- KH Was: = (visited,          sequence ), setElementOf vertex visited
{-exs_6--}    | otherwise = (visited', vertex: sequence') -- KH
{-exs_6--}    -- KH Was: = (visited', vertex: sequence'), otherwise
{-exs_6--}      where
{-exs_6--}      (visited', sequence')
{-exs_6--}       = depthFirstSearch relation
{-exs_6--}                          (setUnion visited (setSingleton vertex), sequence)
{-exs_6--}                          (relation vertex)
{-exs_6--}spanningSearch   :: Ord a =>
{-exs_6--}                    (a -> [a])       -> -- The map
{-exs_6--}                    (Set a, [Set a]) -> -- Current state: visited set,
{-exs_6--}                                        --  current sequence of vertice sets
{-exs_6--}                    [a]              -> -- Input sequence of vertices
{-exs_6--}                    (Set a, [Set a])    -- Final state
{-exs_6--}spanningSearch
{-exs_6--} = foldll . search
{-exs_6--}   where
{-exs_6--}   search relation (visited, setSequence) vertex
{-exs_6--}    | setElementOf vertex visited = (visited,          setSequence ) -- KH
{-exs_6--}    -- KH Was: = (visited,          setSequence ), setElementOf vertex visited
{-exs_6--}    | otherwise = (visited', setFromList (vertex: sequence): setSequence) -- KH
{-exs_6--}    -- KH Was: = (visited', setFromList (vertex: sequence): setSequence)
{-exs_6--}      where
{-exs_6--}      (visited', sequence)
{-exs_6--}       = depthFirstSearch relation
{-exs_6--}                          (setUnion visited (setSingleton vertex), [])
{-exs_6--}                          (relation vertex)
{-exs_6--}scc :: Ord a =>
{-exs_6--}       (a -> [a]) -> -- The "ins"  map
{-exs_6--}       (a -> [a]) -> -- The "outs" map
{-exs_6--}       [a]        -> -- The root vertices
{-exs_6--}       [Set a]       -- The topologically sorted components
{-exs_6--}scc ins outs
{-exs_6--} = spanning . depthFirst
{-exs_6--}   where depthFirst = second . depthFirstSearch outs (setEmpty, [])
{-exs_6--}         spanning   = second . spanningSearch   ins  (setEmpty, [])
{-exs_6--}dependency :: CoreProgram -> CoreProgram
{-exs_6--}dependency =  depends . freeVars
runD = pprint . dependency . parse
{-exs_6--}depends :: AnnProgram Name (Set Name) -> CoreProgram
{-exs_6--}depends prog = [(name,args, depends_e rhs) | (name, args, rhs) <- prog]
{-exs_6--}depends_e :: AnnExpr Name (Set Name) -> CoreExpr
{-exs_6--}depends_e (free, ANum n)          = ENum n
{-exs_6--}depends_e (free, AConstr t a)     = EConstr t a
{-exs_6--}depends_e (free, AVar v)          = EVar v
{-exs_6--}depends_e (free, AAp e1 e2)       = EAp (depends_e e1) (depends_e e2)
{-exs_6--}depends_e (free, ACase body alts) = ECase (depends_e body)
{-exs_6--}                                          [ (tag, args, depends_e e)
{-exs_6--}                                          | (tag, args, e) <- alts
{-exs_6--}                                          ]
{-exs_6--}depends_e (free, ALam ns body)    = ELam ns (depends_e body)
{-exs_6--}depends_e (free, ALet is_rec defns body)
{-exs_6--} = foldr (mkDependLet is_rec) (depends_e body) defnGroups
{-exs_6--}   where
{-exs_6--}   binders    = bindersOf defns
{-exs_6--}   binderSet | is_rec    = setFromList binders
{-exs_6--}             | otherwise = setEmpty
{-exs_6--}   edges      = [(n, f) | (n, (free, e)) <- defns,
{-exs_6--}                          f <- setToList (setIntersection free binderSet)]
{-exs_6--}   ins  v     = [u | (u,w) <- edges, v==w]
{-exs_6--}   outs v     = [w | (u,w) <- edges, v==u]
{-exs_6--}   components = map setToList (scc ins outs binders)
{-exs_6--}   defnGroups = [ [ (n, aLookup defns n (error "defnGroups"))
{-exs_6--}                  | n <- ns]
{-exs_6--}                | ns <- components
{-exs_6--}                ]
{-exs_6--}mkDependLet is_rec dfs e = ELet is_rec [(n, depends_e e) | (n,e) <- dfs] e
