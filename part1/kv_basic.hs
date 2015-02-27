-- A little TCP server that supports a request-response protocol.
-- Implements an in-memory key-value dictionary.
-- One action per connection, no transactionality, no garbage collection.
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import System.IO (hGetLine, hPutStr, hFlush, hClose)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
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
  socket <- listenOn $ PortNumber 1234
  loop socket []

loop :: Socket -> [(Key, Value)] -> IO b
loop socket ref = do
  (connection,_,_) <- accept socket
  command <- hGetLine connection
  let (ref',response) = case (readMaybe command) of {
     Nothing -> (ref,INVALID)
    ;Just PING -> (ref,PONG)
    ;Just (RETRIEVE key) -> (ref,VALUE (retrieve key ref))
    ;Just (STORE key value) -> (\(x,y)-> (x,OK y)) (store (key,value) ref) 
    }
  hPutStr connection $ show response
  hFlush connection
  hClose connection
  loop socket ref'

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
