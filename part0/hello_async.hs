import Control.Concurrent (forkIO)
import Control.Monad (forever)
import System.IO
import Network

main = do
  socket <- listenOn $ PortNumber 1234
  forever $ loop socket

loop socket = do
  (connection,_,_) <- accept socket
  forkIO $ do
    _ <- hGetLine connection
    hPutStr connection "world\r\n"
    hFlush connection
    hClose connection
