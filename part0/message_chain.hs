import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan

nThreads = 10
threadsSayHi = True

main = do
	output  <- newEmptyTMVarIO
	tvars   <- replicateM nThreads newEmptyTMVarIO
	console <- serialIO
	let chain = makeChain output tvars
	    input = head tvars
	mapM (\(n, (l, r)) -> (forkIO (forever (passAlong console n l r))))
		 (zip [1..] chain)
	atomically (putTMVar input (putStrLn "Whee!"))
	atomically (takeTMVar output >>= writeTChan console)
	atomically (putTMVar input (putStrLn "Hello STM!"))
	atomically (takeTMVar output >>= writeTChan console)
	waitEmpty console

passAlong :: TChan (IO ()) -> Int -> TMVar a -> TMVar a -> IO ()
passAlong console n left right = 
	atomically (takeTMVar left >>= putTMVar right >> sayHi)
	where
		sayHi   = if threadsSayHi then sayHi' else return ()
		sayHi'  = writeTChan console (putStrLn message)
		message = "Thread " ++ show n ++ " says hi!"

makeChain :: a -> [a] -> [(a, a)]
makeChain last [x] = [(x, last)]
makeChain last (x1:x2:xs) = (x1, x2) : makeChain last (x2:xs)

serialIO :: IO (TChan (IO ()))
serialIO = do
	console <- newTChanIO
	(forkIO . forever . join . atomically . readTChan) console
	return console

waitEmpty :: TChan a -> IO ()
waitEmpty chan =
	atomically (do
		empty <- isEmptyTChan chan
		if not empty then retry else return ())
