import Control.Concurrent (threadDelay, forkIO)
import System.IO (hGetLine, hPutStr, hFlush, hClose)
import Network

main = withSocketsDo $ do
  socket <- listenOn $ PortNumber 1234
  loop socket

loop :: Socket -> IO ()
loop socket = do
  (connection,_,_) <- accept socket
  _ <- hGetLine connection
  hPutStr connection "world\r\n"
  hFlush connection
  hClose connection
  loop socket
