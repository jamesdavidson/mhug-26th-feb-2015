-- A little TCP server that supports a request-response protocol.
-- One action per connection, basic transactionality, no garbage collection.
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import System.IO (hGetLine, hPutStr, hFlush, hClose)
import Control.Concurrent (forkIO)
import Text.Read (readMaybe)
import Network

type Key = String
type Value = String
type TransactionId = Integer

type Triple = (Key,Value,TransactionId)

data Request = PING
             | RETRIEVE Key TransactionId
             | STORE Key Value deriving (Read,Show)

data Response = PONG
              | OK TransactionId
              | INVALID
              | VALUE Value deriving (Read)

main :: IO ()
main = withSocketsDo $ do
  ref <- newIORef []
  socket <- listenOn $ PortNumber 1234
  loop socket ref

loop :: Socket -> IORef [Triple] -> IO ()
loop socket ref = do
  (connection,_,_) <- accept socket
  command <- hGetLine connection
  response <- case (readMaybe command) of
    Nothing -> do
      return INVALID
    Just PING -> do
      return PONG
    Just (RETRIEVE key transaction) -> do
      readIORef ref >>= return . VALUE . retrieve key transaction
    Just (STORE key value) -> do
      atomicModifyIORef' ref (store (key,value)) >>= return . OK
  hPutStr connection $ show response
  hFlush connection
  hClose connection
  loop socket ref

retrieve :: Key -> TransactionId -> [Triple] -> Value
retrieve k t [] = ""
retrieve k t ((k',v',t'):triples)
  | k == k' = if t' > t then retrieve k t triples else v'
  | otherwise = retrieve k t triples

store :: (Key,Value) -> [Triple] -> ([Triple],TransactionId)
store (k,v) [] = ([(k,v,0)],0)
store (k,v) state = ((k,v,last_tx+1):state,(last_tx+1))
  where (_,_,last_tx) = (head state)

instance Show Response where
  show PONG = "PONG\r\n"
  show (OK tx) = "OK " ++ (show tx) ++ "\r\n"
  show INVALID = "INVALID\r\n"
  show (VALUE s) = s ++ "\r\n"
