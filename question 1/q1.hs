approx
  :: Fractional a
  => (a1 -> a) -> [a1] -> [a] -> a
  
approx f xs ws =
  sum [ w * f x | (x, w) <- zip xs ws ]

 
integrateClosed
  :: Fractional a
  => a -> [a] -> (a -> a) -> a -> a -> Int -> a
integrateClosed v vs f a b n = approx f xs ws * h / v
  where
    m = fromIntegral (length vs - 1) * n
    h = (b - a) / fromIntegral m
    ws = overlap n vs
    xs =
      [ a + h * fromIntegral i | i <- [0 .. m] ]
 
overlap
  :: Num a
  => Int -> [a] -> [a]
overlap n [] = []
overlap n (x:xs) = x : inter n xs
  where
    inter 1 ys = ys
    inter n [] = x : inter (n - 1) xs
    inter n [y] = (x + y) : inter (n - 1) xs
    inter n (y:ys) = y : inter n ys
 
uncurry4 :: (t1 -> t2 -> t3 -> t4 -> t) -> (t1, t2, t3, t4) -> t
uncurry4 f ~(a, b, c, d) = f a b c d
 
-- TEST ----------------------------------------------------------------------

ms = [("simpson", integrateClosed 3 [1, 4, 1])]
 
integrations :: (Fractional a, Num t, Num t1, Num t2)
             => [(String, (a -> a, t, t1, t2))]

integrations =
  [ ("x^3", ((^ 3), 0, 1, 100))
  , ("1/x", ((1 /), 1, 100, 1000))
  , ("x", (id, 0, 5000, 500000))
  , ("x", (id, 0, 6000, 600000))
  ]
 
main :: IO ()
main =
  mapM_
    (\(s, e@(_, a, b, n)) -> do
       putStrLn
         (concat
            [ indent 20 ("f(x) = " ++ s)
            , show [a, b]
            , "  ("
            , show n
            , " approximations)"
            ])
       mapM_
         (\(s, integration) ->
             putStrLn (indent 20 (s ++ ":") ++ show (uncurry4 integration e)))
         ms
       putStrLn [])
    integrations
  where
    indent n = take n . (++ replicate n ' ')