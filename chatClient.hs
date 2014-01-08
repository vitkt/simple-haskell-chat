module ChatClient where 
import Data.Bits
import Network.Socket
import Network.BSD
import Network (withSocketsDo)
import Data.List
import System.IO


connectToServer :: HostName -> String -> IO Handle
connectToServer hostname port  = withSocketsDo $
    do 
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
	   
       let serveraddr = head addrinfos

       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       setSocketOption sock KeepAlive 1
       connect sock (addrAddress serveraddr)
       h <- socketToHandle sock ReadWriteMode
       hSetBuffering h LineBuffering
       return h

sendMessage :: Handle -> String -> IO ()
sendMessage handler msg = hPutStrLn handler msg >> hFlush handler


closeClient :: Handle -> IO ()
closeClient handler = hClose handler
