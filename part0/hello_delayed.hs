import Control.Concurrent (threadDelay, forkIO)
import System.Random
import System.IO
import Network

main = do
  socket <- listenOn $ PortNumber 1234
  loop socket

loop socket = do
  (connection,_,_) <- accept socket
  _ <- forkIO $ do
    _ <- hGetLine connection
    randomRIO (0,3) >>= threadDelay . (*one_second)
    hPutStr connection "world\r\n"
    hFlush connection
    hClose connection
  loop socket

one_second = 1000000
