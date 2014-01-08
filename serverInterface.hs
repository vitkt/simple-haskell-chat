import ChatServer

main = do
	putStrLn "Welcome to Haskell Char Server!"
	putStrLn "Enter port number:"
	portNumber <- getLine
	putStrLn "Starting server..."
	startServer portNumber 
			