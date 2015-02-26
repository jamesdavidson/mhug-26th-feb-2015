-- A little TCP server that supports a request-response protocol.
-- Implements an in-memory key-value dictionary.
-- One action per connection, no transactionality, no garbage collection.
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import System.IO (hGetLine, hPutStr, hFlush, hClose)
import Control.Concurrent (forkIO)
import Text.Read (readMaybe)
import Network

type Key = String
type Value = String

data Request = PING
             | RETRIEVE Key
             | STORE Key Value deriving (Read,Show)

data Response = PONG
              | VALUE Value
              | OK Integer
              | INVALID deriving (Read)

main = withSocketsDo $ do
  ref <- newIORef []
  socket <- listenOn $ PortNumber 1234
  loop socket ref

loop :: Socket -> IORef [(Key,Value)] -> IO ()
loop socket ref = do
  (connection,_,_) <- accept socket
  command <- hGetLine connection
  response <- case (readMaybe command) of
    Nothing -> do
      return INVALID
    Just PING -> do
      return PONG
    Just (RETRIEVE key) -> do
      readIORef ref >>= return . VALUE . retrieve key
    Just (STORE key value) -> do
      atomicModifyIORef' ref (store (key,value)) >>= return . OK
  hPutStr connection $ show response
  hFlush connection
  hClose connection
  loop socket ref

retrieve :: Key -> [(Key,Value)] -> Value
retrieve k [] = ""
retrieve k ((k',v'):pairs)
  | k == k' = v'
  | otherwise = retrieve k pairs

store :: (Key,Value) -> [(Key,Value)] -> ([(Key,Value)],Integer)
store (k,v) pairs = ((k,v):pairs,0)

instance Show Response where
  show PONG = "PONG\r\n"
  show (OK tx) = "OK\r\n"
  show INVALID = "INVALID\r\n"
  show (VALUE s) = s ++ "\r\n"
