solveRPN :: String -> Float
solveRPN = head . foldl f [] . words where
	f (x:xs) "id"    = (id x):xs
	f (x:xs) "acos"  = (acos x):xs
	f (x:xs) "acosh" = (acosh x):xs
	f (x:xs) "asin"  = (asin x):xs
	f (x:xs) "asinh" = (asinh x):xs
	f (x:xs) "atan"  = (atan x):xs
	f (x:xs) "atanh" = (atanh x):xs
	f (x:xs) "cos"   = (cos x):xs
	f (x:xs) "cosh"  = (cosh x):xs
	f (x:xs) "exp"   = (exp x):xs
	f (x:xs) "ln"    = (log x):xs
	f (x:xs) "sin"   = (sin x):xs
	f (x:xs) "sinh"  = (sinh x):xs
	f (x:xs) "sqrt"  = (sqrt x):xs
	f (x:xs) "tan"   = (tan x):xs
	f (x:xs) "tanh"  = (tanh x):xs
	f (x:y:ys) "+"   = (y + x):ys
	f (x:y:ys) "-"   = (y - x):ys
	f (x:y:ys) "*"   = (y * x):ys
	f (x:y:ys) "/"   = (y / x):ys
	f (x:y:ys) "^"   = (y ** x):ys
	f (x:y:ys) "log" = (logBase y x):ys
	f xs "sum"       = [sum xs]
	f xs "prod"      = [product xs]
	f xs "avg"       = [(sum xs)/(fromIntegral . length $ xs)]
	f xs "max"       = [maximum xs]
	f xs "min"       = [minimum xs]
	f xs n           = (read n):xs

main :: IO ()
main = do
	putStrLn "rpncalc: a reverse-Polish-notation calculator for the console."
	putStrLn "Type an expression in RPN and press enter to evaluate."
	putStrLn "Press [ctrl] + [d] to exit."
	interact $ unlines . map (show . solveRPN) . lines
