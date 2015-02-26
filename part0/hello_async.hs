import Control.Concurrent (forkIO)
import System.IO
import Network

main = do
  socket <- listenOn $ PortNumber 1234
  loop socket

loop socket = do
  (connection,_,_) <- accept socket
  _ <- forkIO $ do
    _ <- hGetLine connection
    hPutStr connection "world\r\n"
    hFlush connection
    hClose connection
  loop socket
