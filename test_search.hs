{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where


--import Debug.Trace (traceShow)
import Data.List (sortBy, genericLength, intersperse)
import Control.Monad (liftM)
import System.IO (hPutStrLn, stderr)
import Control.DeepSeq (NFData, rnf)
import Control.Parallel.Strategies (parMap, rdeepseq)

a0 :: Double
a0 = 0

b0 :: Double
b0 = 15
 
costConstant :: Double
costConstant = 100

constantCost :: Cost a Double
constantCost _ _ = const costConstant

type Cost a b = a -> a -> a -> b

linearCost :: Cost Double Double
linearCost _ _ t = (t + 1) * costConstant

quadraticCost :: Cost Double Double
quadraticCost x y t = l' * l'
  where l' = linearCost x y t

reverseLinearCost :: Cost Double Double
reverseLinearCost x y t = (((b0 - a0) * costConstant) -) $ linearCost x y t

-- cost for the "throwing eggs off of different floors" problem
-- if t < s then 0 else 1
-- the expected cost is: P(t >= s | s in [x, y])
-- which for uniform distribution is equal to: 
-- (t - x + 1) / (y - x + 1)
eggsCost :: Cost Double Double
eggsCost x y t =  costConstant * (t - x + 1) / (y - x + 1)


printGraph :: (Fractional a, Ord a, Show a) => Cost Double a -> String -> IO ()
printGraph cost label = do
  let x = makeTrees cost a0 b0 
      rangeStr = spaceConcat ["[", show a0, ",", show b0, "]"]
      
  hPutStrLn stderr $ spaceConcat ["solving:", label, rangeStr]
  let avgCost = averageCost cost x
      labelStr = spaceConcat [label, rangeStr, "(expected cost =", show avgCost, ")"]
  putStr "digraph G {\n"
  putStr $ tree2dot x 
  putStr $ concat ["labelloc=\"t\";label=\"", labelStr, "\""]
  putStr "}\n"
  
main :: IO ()
main = do
  mapM_ (uncurry printGraph) [
    (constantCost, "Constant cost"),
    (linearCost, "Linear cost"),
    (quadraticCost, "Quadratic cost"),
    (reverseLinearCost, "Reverse linear cost"),
    (eggsCost, "Eggs cost")]
  return ()

allPairs :: Monad m => m a -> m b -> m (a,b)
allPairs = (=<<) . flip (liftM . flip (,))

data Tree a = Leaf { getValue :: a } | Node { getRange :: (a,a,a), 
                                              getLeft :: Tree a, 
                                              getRight :: Tree a }
            deriving (Show)
                     
instance NFData a => NFData (Tree a) where
  rnf (Leaf x) = rnf x
  rnf (Node rng l r) = rnf rng `seq` rnf l `seq` rnf r

makeTrees :: (NFData t, Ord t, Enum t, Eq t, Fractional a, Num t, Ord a) => Cost t a -> t -> t -> Tree t
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
                
                
averageCost :: Fractional b => Cost t b -> Tree t -> b
averageCost cost tree = sum d / genericLength d
  where d = calcCosts cost tree
        
sortWith :: Ord a1 => (a -> a1) -> [a] -> [a]
sortWith f = sortBy $ \a b -> compare (f a) (f b)

sortByAverageCost :: (Fractional a, Ord a) => (t -> t -> t -> a) -> [Tree t] -> [Tree t]
sortByAverageCost f = sortWith $ averageCost f

depth :: (Num b, Ord b) => Tree t -> b
depth (Leaf _) = 1 
depth (Node _ l r) = 1 + max (depth l) (depth r)
                     
depths :: Num b => Tree t -> [b]
depths (Leaf _) = [1] 
depths (Node _ l r) = addDepth l ++ addDepth r
  where addDepth x = map (+1) (depths x) 

-- given a cost function and a tree, returns a list of the costs of all nodes in the tree
calcCosts :: Num b => Cost t b -> Tree t -> [b]
calcCosts _    (Leaf _) = [0] 
calcCosts cost (Node (x,t,y) l r) = addCost l ++ addCost r
  where addCost subTree = map (+(cost x y t)) (calcCosts cost subTree) 


averageDepth :: Fractional a => Tree t -> a
averageDepth tree = sum d / genericLength d
  where d = depths tree

sortByAverageDepth :: [Tree t] -> [Tree t]
sortByAverageDepth = sortWith averageDepth

spaceConcat :: [String] -> String
spaceConcat strs = concat $ intersperse " " strs

quote :: String -> String
quote s = "\"" ++ s ++ "\""

nodeLabel :: (RealFrac a, Show a) => Tree a -> String
nodeLabel (Leaf x) = quote . show $ x
nodeLabel (Node (_,x,_) _ _) = quote $ show (floor x) ++ " ?"

tree2dot :: (RealFrac a, Show a) => Tree a -> [Char]
tree2dot t@(Leaf _) = nodeLabel t ++ ";\n"
tree2dot t@(Node _ l r) = nodeLabel t ++ " -> " ++ nodeLabel l' ++ ";\n" ++ nodeLabel t ++ " -> " ++ nodeLabel r' ++ ";\n" ++ tree2dot l' ++ tree2dot r' 
  where orderNodes a'@(Leaf a) b'@(Leaf b) = orderByLabels a b a' b'
        orderNodes a'@(Leaf a) b'@(Node (_, b, _) _ _) = orderByLabels a b a' b'
        orderNodes a'@(Node (_, a, _) _ _) b'@(Node (_, b, _) _ _) = orderByLabels a b a' b'
        orderNodes a'@(Node (_, a, _) _ _) b'@(Leaf b) = orderByLabels a b a' b'
        orderByLabels a b a' b' = if a <= b then (a', b') else (b', a')
        l' = fst $ orderNodes l r
        r' = snd $ orderNodes l r

