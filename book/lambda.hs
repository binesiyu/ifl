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

runD = pprint . dependency . parse
