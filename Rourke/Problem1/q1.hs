{- base function to be integrated -}
func :: Double -> Double
func x = log x

{- recursively calculate final output based on the simpsons 1/3 rule -}
calcResult :: Int -> Int -> Double -> [Double] -> Double
calcResult i n result fx | (i > n) = (result)
                         | (i == 0 || i == n) = calcResult (i+1) n (result + fx!!i) fx
                         | (i `mod` 2 /= 0) = calcResult (i+1) n (result + 4*fx!!i) fx
                         | otherwise = calcResult (i+1) n (result + 2*fx!!i) fx
                        
{- main program loop -}

main :: IO()
main = do
    {- read inputs from user -}

    putStr "input n: "
    nTemp <- getLine
    let n = read nTemp :: Int
    let nDouble = read nTemp :: Double

    
    putStr "input lower limit: "
    llTemp <- getLine
    let ll = read llTemp :: Double
    
    putStr "input upper limit: "
    ulTemp <- getLine
    let ul = read ulTemp :: Double

    {- calculate value of h -}
    let h = (ul - ll)/nDouble

    {- generate list of values to be integrated with based on the n number rectangles-}
    let xTemp = map (*h)[0..nDouble]
    let x = map (+ll) xTemp
    let fx = map func x 
    
    {- calculate and display final result -}
    let y = calcResult 0 n 0 fx
    print(y*(h/3))
    