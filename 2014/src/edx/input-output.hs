main = do
	putStrLn "hello,whats your name here"
	name <- getLine
	putStrLn $ "hello" ++ name 

