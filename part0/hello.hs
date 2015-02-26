import System.IO
import Network

main = do
  socket <- listenOn (PortNumber 1234)
  loop socket

loop socket = do
  (connection,_,_) <- accept socket
  hPutStr connection "hello world\r\n"
  hFlush connection
  hClose connection
  loop socket

