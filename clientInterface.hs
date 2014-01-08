import Data.Bits
import Network.Socket
import Network.BSD
import Network (withSocketsDo)
import Data.List
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar

import ChatClient
import GlobalFunctions
readForever h = hGetLine h >>= \content -> putStrLn content >> readForever h;
loopGet nick handler = print (nick ++ " >") >> getLine >>= \a -> (sendMessage handler (show (1,a))) >> loopGet nick handler;

getNick connectionHandler = do
	nick <-getLine
	sendMessage connectionHandler (show (0,nick))
	answer <- hGetLine connectionHandler 
	let nickResult = read answer :: (Int,String)
	case nickResult of 
		(2,"OK") -> putStrLn ("Auth succsess, " ++ nick++" !") >> return nick;
		(2,"NO") -> putStrLn "Your nick is exist, please re-enter:" >> getNick connectionHandler;


main = do
putStrLn "Welcome to Haskell chat!"
putStrLn "Enter ip-address:"
ipAddr <- getLine
putStrLn "Enter port:"
port <- getLine
connectionHandler <- connectToServer ipAddr port
putStrLn "Enter yor nick name:"
nick <- getNick connectionHandler
forkIO $ readForever connectionHandler
loopGet nick connectionHandler
return ()
