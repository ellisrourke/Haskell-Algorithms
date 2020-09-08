{- Question 3 -}

import Data.List

{- extract only the first values from a list of tuples -}
getFirst :: [(a,b)] -> [a]
getFirst lst = [fst x | x <- lst]

{- extract only the second values from a list of tuples -}
getLast :: [(a,b)] -> [b]
getLast lst = [snd x | x <- lst]

listIntersect :: Ord a => [a] -> [a] -> Int
listIntersect x xs = listIntersect' (sort x) (sort xs)

listIntersect' [] _ = 0
listIntersect' _ [] = 0
listIntersect' l@(a: as) r@(b:bs) | a < b = listIntersect' as r
                                  | a == b = listIntersect' as bs+1
                                  | otherwise = listIntersect' l bs

{- convert user input into a tuple of two values, split on the first space -}
strToTuple :: [String] -> [(String,String)]
strToTuple = map toTuple

toTuple :: String -> (String,String)
toTuple str = join $ words str

join :: [a] -> (a,a)
join [x,y] = (x,y)
joing _ = error "null"

{- get n values of input from a user -}
getInputList :: Int -> IO [String]
getInputList n
            | n <= 0 = return []
            | otherwise = do
                x <- getLine
                xs <- getInputList(n-1)
                return (x:xs)

{- main program loop -}    
main :: IO()
main = do  
    n <- getLine
    let numPlugs = read n :: Int
    plugs <- getInputList numPlugs
    {-let plugs = ["A","B","C","D"]-}
    
    
    m <- getLine
    let numDevices = read m :: Int
    devices <- getInputList numDevices
    let formattedDevices = strToTuple devices 
    let finalDevices = getLast formattedDevices
    {-let devices = [("Laptop","B"),("Phone","C"),("Pager","B"),("Clock","B"),("Comb","X")]-}
    
    k <- getLine
    let numAdaptors = read k :: Int
    adapters <- getInputList numAdaptors
    
    let formattedAdaptors = strToTuple adapters
    
    let validAdapters= filter (\(x,y) -> (y `elem` plugs)) formattedAdaptors
    
    let strippedAdapters = getFirst validAdapters
    {-let adapters = [("B","X"),("X","A"),("X","D")]-}

    print(listIntersect strippedAdapters finalDevices)
    
    

