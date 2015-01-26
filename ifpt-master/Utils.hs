module Utils where

import qualified Data.List as L (lookup, sort)

shownum n = show n
hd = head
tl :: [a] -> [a]
tl = tail
zip2::[a] ->[b] -> [(a,b)]
zip2 = zip

data Heap a = Heap Int [Int] [(Int, a)]
type Addr = Int


hInitial::Heap a
hInitial = Heap 0 [1..] []
hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap size (next:free) cts) n = (Heap (size+1) free ((next, n):cts), next)
hUpdate::Heap a -> Addr -> a -> Heap a
hUpdate (Heap size free cts) a n = Heap size free ((a,n):remove cts a)
hFree::Heap a -> Addr -> Heap a
hFree (Heap size free cts) a = Heap (size - 1) (a:free) (remove cts a)
hLookup :: Heap a -> Addr -> a
hLookup (Heap size free cts) a = aLookup cts a (error ("cannot find node" ++ showaddr a ++ " in heap"))
hAddresses:: Heap a -> [Addr]
hAddresses (Heap size free cts) = [addr | (addr, node) <- cts]
hSize :: Heap a -> Int
hSize (Heap size free cts) = size
hNull :: Addr
hNull = 0
hIsnull:: Addr -> Bool
hIsnull a = a == hNull
showaddr :: Addr -> [Char]
showaddr a = "#" ++ shownum a

remove :: [(Int, a)] -> Int -> [(Int, a)]
remove [] a = error ("Attempt to update or free nonexistent address #" ++ shownum a)
remove ((a',n):cts) a
  | a == a' = cts
  | a /= a' = (a', n):remove cts a


-- A.2
type ASSOC a b = [(a,b)]
aLookup ::Eq a => ASSOC a b -> a -> b -> b
aLookup as a def = case L.lookup a as of
  Just x ->  x
  Nothing -> def

aDomain::ASSOC a b -> [a]
aDomain as = [key | (key, val) <- as]
aRange :: ASSOC a b -> [b]
aRange alist = [val | (key, val) <- alist]
aEmpty = []

-- A.3
type NameSupply = Int
getName::NameSupply -> String -> (NameSupply, String)
getName name_supply prefix = (name_supply + 1, makeName prefix name_supply)
getNames::NameSupply -> [String] -> (NameSupply, [String])
getNames name_supply prefixes
  = (name_supply + length prefixes, zipWith makeName prefixes [name_supply ..])
makeName prefix ns = prefix ++ "_" ++ shownum ns
initialNameSupply::NameSupply
initialNameSupply = 0

-- A.4


type Set a = [a]
setFromList::(Ord a) => [a] -> Set a
setFromList = rmdup . L.sort
  where
    rmdup [] = []
    rmdup [x] = [x]
    rumdup (x:y:xs) | x == y = rmdup (y:xs)
                    | otherwise = x:rmdup (y:xs)
setToList :: (Ord a) => Set a -> [a]
setToList xs = xs
setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (a:as) (b:bs) | a < b = a:setUnion as (b:bs)
                        | a == b = a:setUnion as bs
                        | a > b = b: setUnion (a:as) bs
setIntersection::(Ord a) => Set a -> Set a -> Set a
setIntersection [] [] = []
setIntersection [] (b:bs) = []
setIntersection (a:as) [] = []
setIntersection (a:as) (b:bs) | a < b = setIntersection as (b:bs)
                              | a == b = a:setIntersection as bs
                              | a > b = setIntersection (a:as) bs

setSubtraction :: (Ord a) => Set a -> Set a -> Set a
setSubtractino [] [] = []
setSubtractino [] (b:bs) = []
setSubtraction (a:as) [] = a:as
setSubtraction (a:as) (b:bs) | a < b = a:setSubtraction as (b:bs)
                             | a == b = a:setSubtraction as bs
                             | a > b = setSubtraction (a:as) bs
setElementOf :: (Ord a) => a -> Set a -> Bool
setElementOf x [] = False
setElementOf x (y:ys) = x == y || (x > y && setElementOf x ys)
setEmpty :: Ord a => Set a
setEmpty = []
setIsEmpty :: (Ord a) => Set a -> Bool
setIsEmpty s = null s
setSingleton::(Ord a) => a -> Set a
setSingleton x = [x]
setUnionList :: (Ord a) => [Set a] -> Set a
setUnionList = foldl setUnion setEmpty

first = fst
second = snd

space n = take n (repeat ' ')


--
