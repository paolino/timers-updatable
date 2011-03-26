-- | An updatable timer is a timer for which it is possible to update the remaining time. 
--
-- Methods are exposed in STM to give composability. IO wrappers for those are exported also. 
--
-- Parallel and serial update politics are implemented.
--
-- In the example we start a timer with a time to wait of 10 seconds, hang 2 threads which will wait for it to finish, and update 
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
  , replacer
  -- * Utility
  , longThreadDelay
  ) where

import Data.List (unfoldr)
import Data.Maybe
import Data.Int (Int64)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when, forever)
import Control.Concurrent.STM
import Control.Concurrent.Killable

-- | A delay in microseconds
type Delay = Int64

-- | Abstract timers that can be updated. Hanging via wait function can be done by any number of threads, which is synchronization.
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

-- | Create and start a parallel updatable timer. The "renew" action for this timer will start parallel timers. The last timer
-- that is over will compute the given action.
parallel  :: IO a             -- ^ the action to run when timer rings 
          -> Delay            -- ^ time to wait
          -> IO (Updatable a) -- ^ the updatable parallel timer
parallel a d0 = do
  tz <- newTVarIO 0
  tp <- newTVarIO []
  let 
    t k = do 
      p <- forkIO $ atomically (readTVar tz >>= writeTVar tz . (+1)) >> longThreadDelay k >> atomically (readTVar tz >>= writeTVar tz . (subtract 1))
      atomically $ readTVar tp >>= writeTVar tp . (p :)
    w = do 
      atomically $ do
        z <- readTVar tz
        when (z > 0) retry
      a 
    k = atomically (readTVar tp) >>= kill 
  engine k t w d0
         
-- | Create and start a serial updatable timer. The "renew" action for this timer will schedule new timer after the running one.
-- The timer will run the given action after the sum of all scheduled times is over.
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
        Just l -> longThreadDelay l >> w
  engine (return ()) t w d0

-- | Create  and start a replacer updatable timer.  The "renew" action for this timer will insert a new timer replacing the running one.
-- The timer will run the given action after this time

replacer  :: IO a             -- ^ the action to run when timer rings 
          -> Delay            -- ^ time to wait
          -> IO (Updatable a) -- ^ the updatable parallel timer
replacer a d0 = do
  tz <- newTVarIO []
  z <- newEmptyTMVarIO 
  let 
    t k = do
      ps <- atomically $ readTVar tz
      mapM_ kill ps
      p <- forkIO $ longThreadDelay k >> atomically (putTMVar z ())
      atomically $ readTVar tz >>= writeTVar tz . (p:)
    w = atomically (takeTMVar z) >> a
  engine (return ()) t w d0


-- | Pause the thread for the given number of microseconds.  There is no guarantee that the thread will be restarted promptly
-- after the delay, but it will not be started before then.
-- 
-- Similar to "threadDelay", but takes a 64-bit argument.  The Haskell 2010 specification says that (maxBound :: Int) is at least 
-- 2^29-1.  However 2^29 microseconds is only about 538 seconds.  GHC on a 32-bit machine has a 32 bit Int, but that is still less
-- than 36 minutes.  64-bit signed integers give a maximum delay of over 292 million years, which should be sufficient.
longThreadDelay :: Delay -> IO ()
longThreadDelay d = mapM_ (threadDelay . fromIntegral) $ unfoldr f d
   where
      f d1 | d1 <= 0     = Nothing
           | d1 < maxInt = Just (d1, 0)
           | otherwise   = Just (maxInt, d1-maxInt)
      maxInt = fromIntegral (maxBound :: Int)  -- Platform-dependent



main = do
 t <- parallel (return 5) $ 10^7
 forkIO $ waitIO t >>= print . (+1) . fromJust 
 forkIO $ waitIO t >>= print . (+2) . fromJust
 threadDelay $ 5 * 10 ^ 6
 renewIO t $ 6 * 10 ^ 6
 waitIO t >>= print . fromJust 

