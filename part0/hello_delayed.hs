import Control.Concurrent (threadDelay, forkIO)
import System.IO (hGetLine, hPutStr, hFlush, hClose)
import System.Random
import Network

main = withSocketsDo $ do
  socket <- listenOn $ PortNumber 1234
  loop socket

loop :: Socket -> IO ()
loop socket = do
  (connection,_,_) <- accept socket
  _ <- forkIO $ do
    _ <- hGetLine connection
    r <- randomRIO (0,4)
    threadDelay (r*1000000) -- one second?
    hPutStr connection "world\r\n"
    hFlush connection
    hClose connection
  loop socket
