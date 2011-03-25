-- | An updatable timer is a timer for which it is possible to update the remaining time. 
--
-- Methods are exposed in STM to give composability. IO wrappers for those are exported also. 
--
-- Parallel and serial update politics are implemented.
--
-- In the example we start a timer with a time to wait of 10 seconds, hang 2 threads which will wait for it to finish and update 
-- it after 5 seconds to wait for other 6 seconds. 
-- It will complete and run its action and the hanged threads after 11 seconds because of its parallel nature. 
-- The serial timer would have ringed after 16 seconds.
--
-- @
-- import Control.Concurrent
-- import System.Timer.Updatable
-- import Data.Maybe
-- main = do
--  t <- parallel (return 5) $ 10^7
--  forkIO $ waitIO t >>= print . (+1) . fromJust 
--  forkIO $ waitIO t >>= print . (+2) . fromJust
--  threadDelay $ 5 * 10 ^ 6
--  renewIO t $ 6 * 10 ^ 6
--  waitIO t >>= print . fromJust 
-- @


module System.Timer.Updatable 
  (Delay 
  -- * Datatype
  , Updatable 
  , wait
  , renew
  -- * IO wrappers
  , waitIO
  , renewIO
  -- * Builders
  , parallel
  , serial
  ) where

import Data.Maybe
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when, void, forever)
import Control.Concurrent.STM
import Control.Concurrent.Killable

-- | A delay in microseconds
type Delay = Int

-- | Abstract timers that can be updated. Hanging via wait function can be done by any number of threads, which is sinchronization.
data Updatable a = Updatable {
  wait :: STM (Maybe a),     -- ^ wait until the timer rings, or signal Nothing if timer is destroyed
  renew :: Delay -> STM (),  -- ^ update the delay in the timer
  _kill :: IO ()
  }

instance Killable (Updatable a) where
  kill  = _kill

-- | Wait in IO 
waitIO :: Updatable a -> IO (Maybe a)
waitIO = atomically . wait

-- | Renew in IO 
renewIO :: Updatable a -> Delay -> IO ()
renewIO u = atomically . renew u

-- wrap the logic with a framework for signalling the time is over 
engine :: IO () -> (Delay -> IO ()) -> IO a -> Delay -> IO (Updatable a)
engine k t w d0 = do
  deltas <- newTChanIO 
  x <- newEmptyTMVarIO
  t d0 
  z <- forkIO . forever $ atomically (readTChan deltas) >>= t
  p <- forkIO $ w >>= atomically . putTMVar x . Just  
  return $ Updatable 
    (takeTMVar x >>= \r -> putTMVar x r >> return r) 
    (writeTChan deltas) 
    (k >> kill [p,z] >> atomically (putTMVar x Nothing))

-- | Create and start a parallel updatable timer. This timer renew actions will start parallel timers. Last timer that is over will compute the given action.
parallel  :: IO a             -- ^ the action to run when timer rings 
          -> Delay            -- ^ time to wait
          -> IO (Updatable a) -- ^ the updatable parallel timer
parallel a d0 = do
  tz <- newTVarIO 0
  tp <- newTVarIO []
  let 
    t k = do 
      p <- forkIO $ atomically (readTVar tz >>= writeTVar tz . (+1)) >> threadDelay k >> atomically (readTVar tz >>= writeTVar tz . (subtract 1))
      atomically $ readTVar tp >>= writeTVar tp . (p :)
    w = do 
      atomically $ do
        z <- readTVar tz
        when (z > 0) retry
      a 
    k = atomically (readTVar tp) >>= kill 
  engine k t w d0
         
-- | Create and start a serial updatable timer. This timer renew action will schedule new timer after the running one. The timer will run the given action after the sum of all scheduled times is over.
serial  :: IO a             -- ^ the action to run when timer rings 
          -> Delay            -- ^ time to wait
          -> IO (Updatable a) -- ^ the updatable parallel timer
serial a d0 = do
  tz <- newTChanIO 
  let 
    t = atomically . writeTChan tz 
    w = do
      l <- atomically $ (Just `fmap` readTChan tz) `orElse` return Nothing
      case l of
        Nothing -> a
        Just l -> threadDelay l >> w
  engine (return ()) t w d0

main = do
 t <- parallel (return 5) $ 10^7
 forkIO $ waitIO t >>= print . (+1) . fromJust 
 forkIO $ waitIO t >>= print . (+2) . fromJust
 threadDelay $ 5 * 10 ^ 6
 renewIO t $ 6 * 10 ^ 6
 waitIO t >>= print . fromJust 

