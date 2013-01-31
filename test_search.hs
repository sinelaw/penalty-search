{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where


import Debug.Trace (traceShow)
import Data.List (minimumBy, sortBy, genericLength)
import Control.Monad ((=<<), liftM, liftM2)
import Data.Maybe (fromJust)

trace1 x = traceShow x x

a0 = 0
b0 = 10
 
s = 2 -- some random value in [a,b]
costConstant = 1

cost = linearCost

test1 = head $ (sortByAverageCost constantCost) . map fromJust $ (makeTrees 0 5)
test2 = head $ (sortByAverageCost linearCost) . map fromJust $ (makeTrees 0 5)
test3 = head $ (sortByAverageDepth) . map fromJust $ (makeTrees 0 5)

main = do
  putStr $ tree2dot test3
  -- putStr "/* */"
  -- putStr $ tree2dot test2
  
--   print $ searchRange (a0,b0) 0 []


test x = x >= s

--allPairs :: [a] -> [b] -> [(a,b)]
allPairs :: Monad m => m a -> m b -> m (a,b)
allPairs = (=<<) . flip (liftM . flip (,))

data Tree a = Leaf { getValue :: a } | Node { getRange :: (a,a,a), 
                                              getLeft :: Maybe (Tree a), 
                                              getRight :: Maybe (Tree a) }
            deriving (Show)

makeTrees :: (Enum a, Eq a, Num a) => a -> a -> [Maybe (Tree a)]
makeTrees a b = if a == b 
                then [Just $ Leaf a]
                else foldr ((++) . makeTree') [] [a..(b-1)]
  where makeTree' x = map (Just . (uncurry $ Node (a,x,b))) 
                      . uncurry allPairs 
                      $ if (x == a) then ([Just $ Leaf a], rightTrees)
                        else if (x == b) then (leftTrees, [Nothing])
                             else (leftTrees, rightTrees)
          where leftTrees = makeTrees a x
                rightTrees = makeTrees (x+1) b
                
                
depth :: (Num b, Ord b) => Tree t -> b
depth (Leaf _) = 1 
depth (Node _ l r) = 1 + max (maybe 0 depth $ l) (maybe 0 depth $ r)
                     
--depths :: (Num b, Ord b, Fractional b) => Tree t -> b
depths (Leaf _) = [1] 
depths (Node _ l r) = addDepth l ++ addDepth r
  where addDepth x = map (+1) (maybe [0] depths x) 

costs f t@(Leaf x) = [f x] 
costs f t@(Node (_,x,_) l r) = addCost l ++ addCost r
  where addCost y = map (+(f x)) (maybe [0] (costs f) y) 


averageDepth tree = sum d / genericLength d
  where d = depths tree

averageCost f tree = sum d / genericLength d
  where d = costs f tree

sortWith f = sortBy (\a b -> (compare (f a) (f b)))

sortByAverageDepth = sortWith averageDepth
sortByAverageCost f = sortWith (averageCost f)


constantCost = const costConstant
linearCost x = (x + 1) * costConstant


nodeLabel (Leaf x) = show x
nodeLabel (Node x _ _) = show x

nodeLabel' :: Show a => Maybe (Tree a) -> String 
nodeLabel' Nothing = "bad"
nodeLabel' (Just x) = nodeLabel x

tree2dot t@(Leaf _) = nodeLabel t ++ ";\n"
tree2dot t@(Node _ l r) = nodeLabel t ++ " -> " ++ nodeLabel' l ++ ";\n" ++ nodeLabel t ++ " -> " ++ nodeLabel' r ++ ";\n" ++ tree2dot' l ++ tree2dot' r 

tree2dot' :: Show a => Maybe (Tree a) -> String 
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

