module ChatServer where
import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import Control.Exception
import Data.IORef

import GlobalFunctions

type HandlerFunc = SockAddr -> String -> IO ()

startServer port = withSocketsDo $ do
	let addrInfoSettings = (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
	parseAddress <- try (getAddrInfo addrInfoSettings Nothing (Just port)) :: IO ( Either SomeException [AddrInfo])
	case parseAddress of
		(Left _ ) -> putStrLn "Bad port number"
		(Right b) -> createServer (head b);

createServer  serveraddr = do 
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       bindSocket sock (addrAddress serveraddr)
       listen sock 5
       lock <- newMVar ()
       clientList <- newIORef ([]::[(String,Handle)])
       procRequests clientList lock sock
    where
          -- | Process incoming connection requests
          procRequests :: IORef [(String,Handle)] -> MVar () -> Socket -> IO ()
          procRequests clients lock mastersock = 
              do (connsock, clientaddr) <- accept mastersock
                 dispatchSystemMessage lock clientaddr "New client connected!"
                 forkIO $ procMessages clients lock connsock clientaddr
                 procRequests clients lock mastersock

          -- | Process incoming messages
          procMessages :: IORef [(String,Handle)] -> MVar () -> Socket -> SockAddr -> IO ()
          procMessages clients lock connsock clientaddr =
              do connhdl <- socketToHandle connsock ReadWriteMode
                 hSetBuffering connhdl LineBuffering
                 nickName <- newIORef ""
                 messages <- hGetContents connhdl
                 mapM_ (dispatchChatMessage clients lock connhdl nickName clientaddr) (lines messages)
                 hClose connhdl
                 dispatchSystemMessage lock clientaddr "Client disconnected!"

          -- Lock the handler before passing data to it.
          dispatchChatMessage :: IORef [(String,Handle)] -> MVar () -> Handle -> IORef String -> HandlerFunc
          dispatchChatMessage clients lock h nick clientaddr msg = withMVar lock (\a -> handlerfunc clients h nick clientaddr msg >> return a)
          
          dispatchSystemMessage :: MVar () -> HandlerFunc
          dispatchSystemMessage lock clientaddr msg = withMVar lock (\a -> putStrLn msg >> return a)	 
	

-- A simple handler that prints incoming packets
handlerfunc :: IORef [(String,Handle)] -> Handle -> IORef String -> HandlerFunc

handlerfunc clients h nickName addr msg = case (read msg) :: (Int,String) of
		(0,nick) -> isUniqueNick clients nick >>= \result -> if not result then acceptNick clients h nickName addr msg nick else wrongNick clients h nickName addr msg nick;
		(1,message) -> readIORef nickName >>= \nName -> readIORef clients >>= \clientsList -> sendToAll clientsList nName (nName++" > "++message)

isUniqueNick clients nick = readIORef clients >>= \clientsList -> return (elem nick [n|(n,h)<-clientsList]);
acceptNick clients h nickName addr msg nick = putStrLn ("Accept nick: " ++ nick)>>(hPutStrLn h (show (2,"OK"))>> hFlush h) >> writeIORef nickName nick >> modifyIORef clients (addElement (nick,h)) >> readIORef clients >>= \clientsList ->  sendToAll clientsList nick ("User \""++nick++"\" was entered in chat");
wrongNick clients h nickName addr msg nick = putStrLn ("Wrong nick: " ++ nick)>>(hPutStrLn h (show (2,"NO"))>> hFlush h);


sendToAll [] fromNick msg = return();
sendToAll ((n,h):lst) fromNick msg = (if (fromNick == n) then return () else (hPutStrLn h msg >> hFlush h) )>> sendToAll lst fromNick msg;