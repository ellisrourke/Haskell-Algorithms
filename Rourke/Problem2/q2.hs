import Control.Monad

{- tree data structure -}
data QTree = 
     Node QTree QTree QTree QTree
    | Empty
    | Full
    deriving(Show)

{- exhaustive cases for merging two quadtrees into a single tree -}
mergeTree Empty Empty = Empty
mergeTree Full Empty = Full
mergeTree Empty Full = Full
mergeTree (Node a b c d) Full = Full
mergeTree Full (Node a b c d) = Full
mergeTree (Node a b c d) Empty = Node a b c d
mergeTree Empty (Node a b c d) = Node a b c d
mergeTree (Node a b c d) (Node a1 b1 c1 d1) = Node (mergeTree a a1) (mergeTree b b1) (mergeTree c c1) (mergeTree d d1)

{- convert user input into quadtree data structure-}
parseQT :: String -> (QTree,String)
parseQT ('e' : cs) = (Empty,cs)
parseQT ('f' : cs) = (Full,cs)
parseQT ('p' : cs) = 
    let (st1,cs1) = parseQT cs
        (st2,cs2) = parseQT cs1
        (st3,cs3) = parseQT cs2
        (st4,cs4) = parseQT cs3
    in (Node st1 st2 st3 st4,cs4)
parseQT "" = error ""

{- calculate the depth of a given tree -}
treeDepth :: QTree -> Int
treeDepth Empty = 0
treeDepth Full = 0
treeDepth (Node a b c d) = 1 + (max (max (treeDepth a)(treeDepth b)) (max(treeDepth c)(treeDepth d)) )

{- calculate the cumlative value of a quadtrees nodes-}
treeSum Empty v = 0
treeSum Full v = v
treeSum (Node a b c d) v | treeDepth (Node a b c d) == 1 = treeSum a 256 + treeSum b 256 + treeSum c 256 + treeSum d 256
                         | otherwise = treeSum a 64 + treeSum b 64 + treeSum c 64 + treeSum d 64

{- main program loop - get user input -}
main :: IO()
main = do  n <- getLine
           let nInt = read n :: Int
           replicateM_ nInt run         

run :: IO()
run = do  putStr "Enter tree a: "
          treeA <- getLine
          let a = fst(parseQT treeA)
          putStr "Enter tree b: "
          treeB <- getLine
          let b = fst(parseQT treeB)
          let m = mergeTree a b
          let z = treeSum m 256
          putStrLn ("there are " ++ show z ++ " black pixels")
          putStrLn "------------------------------------------"
