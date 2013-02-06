{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where


import Debug.Trace (traceShow)
import Data.List (minimumBy, sortBy, genericLength)
import Control.Monad ((=<<), liftM, liftM2)
import Data.Maybe (fromJust, isJust)
import System.IO (hPutStrLn, stderr)
import Control.Parallel (par, pseq)
import Control.DeepSeq (NFData, rnf)
import Control.Parallel.Strategies (parMap, rdeepseq)

a0 = 0 :: Double
b0 = 16 :: Double
 
costConstant = 1000

constantCost = const costConstant
linearCost x = (x + 1) * costConstant
quadraticCost x = (linearCost x) * (linearCost x)

cost = linearCost

--testTrees = makeTrees a0 $! b0
-- test1 = head $ (sortByAverageCost constantCost) . map fromJust $ testTrees 
-- test2 = head $ (sortByAverageCost linearCost) . map fromJust $ testTrees
-- test3 = head $ (sortByAverageCost quadraticCost) . map fromJust $ testTrees
--test4 = head $ (sortByAverageDepth) . map fromJust $ testTrees
test1 = makeTrees constantCost a0 b0
test2 = makeTrees linearCost a0 b0
test3 = makeTrees quadraticCost a0 b0
  
printGraph x label = do
  let rangeStr = "[" ++ show a0 ++ ", " ++ show b0 ++ "]"
  hPutStrLn stderr $ "solving: " ++ label ++ " - " ++ rangeStr
  putStr "digraph G {\n"
  putStr $ tree2dot x 
  putStr $ "labelloc=\"t\";label=\"" ++ label ++ " - " ++ rangeStr ++ "\";"
  putStr "}\n"
  
main = do
  printGraph test1 "Constant cost"
  printGraph test2 "Linear cost"
  printGraph test3 "Quadratic cost"

allPairs :: Monad m => m a -> m b -> m (a,b)
allPairs = (=<<) . flip (liftM . flip (,))

data Tree a = Leaf { getValue :: a } | Node { getRange :: (a,a,a), 
                                              getLeft :: Tree a, 
                                              getRight :: Tree a }
            deriving (Show)
                     
instance NFData a => NFData (Tree a) where
  rnf (Leaf x) = rnf x
  rnf (Node rng l r) = rnf rng `seq` rnf l `seq` rnf r

makeTrees :: (NFData t, Ord t, Enum t, Eq t, Fractional a, Num t, Ord a) => (t -> a) -> t -> t -> Tree t
makeTrees cost a b = if a == b 
                     then Leaf a
                     else head . (sortByAverageCost cost) $ map' makeTree' [a..(b-1)]
  where makeTree' x = uncurry (Node (a,x,b))
                      $ if (x == a) 
                        then (Leaf a, rightTree)
                        else (rightTree, leftTree)
          where leftTree = makeTrees cost a x
                rightTree = makeTrees cost (x+1) b
        map' = if (b - a) > 5 -- use parallel map only for larger trees
               then parMap rdeepseq
               else map
                
                
averageCost :: Fractional b => (t -> b) -> Tree t -> b
averageCost f tree = sum d / genericLength d
  where d = costs f tree
        
sortWith :: Ord a1 => (a -> a1) -> [a] -> [a]
sortWith f = sortBy $ \a b -> compare (f a) (f b)

sortByAverageCost :: (Fractional a, Ord a) => (t -> a) -> [Tree t] -> [Tree t]
sortByAverageCost f = sortWith $ averageCost f


depth :: (Num b, Ord b) => Tree t -> b
depth (Leaf _) = 1 
depth (Node _ l r) = 1 + max (depth l) (depth r)
                     
--depths :: (Num b, Ord b, Fractional b) => Tree t -> b
depths (Leaf _) = [1] 
depths (Node _ l r) = addDepth l ++ addDepth r
  where addDepth x = map (+1) (depths x) 

costs f t@(Leaf x) = [f x] 
costs f t@(Node (_,x,_) l r) = addCost l ++ addCost r
  where addCost y = map (+(f x)) ((costs f) y) 


averageDepth tree = sum d / genericLength d
  where d = depths tree

sortByAverageDepth = sortWith averageDepth



quote s = "\"" ++ s ++ "\""

nodeLabel (Leaf x) = quote . show $ x
nodeLabel (Node (a,b,c) _ _) = quote $ show (floor b) ++ " ?"

nodeLabel' :: (Show a, RealFrac a) => Maybe (Tree a) -> String 
nodeLabel' Nothing = "bad"
nodeLabel' (Just x) = nodeLabel x

tree2dot t@(Leaf _) = nodeLabel t ++ ";\n"
tree2dot t@(Node _ l r) = nodeLabel t ++ " -> " ++ nodeLabel l' ++ ";\n" ++ nodeLabel t ++ " -> " ++ nodeLabel r' ++ ";\n" ++ tree2dot l' ++ tree2dot r' 
  where orderNodes a'@(Leaf a) b'@(Leaf b) = orderByLabels a b a' b'
        orderNodes a'@(Leaf a) b'@(Node (_, b, _) _ _) = orderByLabels a b a' b'
        orderNodes a'@(Node (_, a, _) _ _) b'@(Node (_, b, _) _ _) = orderByLabels a b a' b'
        orderNodes a'@(Node (_, a, _) _ _) b'@(Leaf b) = orderByLabels a b a' b'
        orderByLabels a b a' b' = if a <= b then (a', b') else (b', a')
        l' = fst $ orderNodes l r
        r' = snd $ orderNodes l r

tree2dot' :: (Show a, RealFrac a) => Maybe (Tree a) -> String 
tree2dot' t@Nothing = nodeLabel' t
tree2dot' (Just x) = tree2dot x
  



-- nextRange (a,b) curX = 
--   if a == b
--   then Nothing
--   else case test curX of
--     False -> if curX == b then Just (b,b) else Just (curX+1, b)
--     True  -> if curX == a then Just (a,a) else Just (a, curX-1)
  
-- searchRange (a,b) curCost path =
--   minimumBy (\(cost1, _) (cost2, _) -> compare cost1 cost2) 
--   $ ranges
--   where
--     ranges = foldr searchRange' [] [a..b]
--     searchRange' x prevXs = case nextRange' of
--       Nothing    -> (curCost, nextPath) : prevXs
--       Just rng   -> (searchRange rng nextCost nextPath) : prevXs
--       where nextRange' = nextRange (a,b) x
--             nextCost = cost x + curCost
--             nextPath = (x, (a,b)) : path

