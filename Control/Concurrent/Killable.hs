{-# LANGUAGE FlexibleInstances #-}
-- | Ad hoc overload of term kill
--
--



module Control.Concurrent.Killable (Killable (kill)) where

import Prelude hiding (mapM_)
import Control.Concurrent (killThread, ThreadId)
import Data.Foldable 

-- | Objects that forked threads and can be killed
class Killable a where
  kill :: a -> IO ()

instance Killable ThreadId where
  kill = killThread

instance Foldable a => Killable (a ThreadId) where
  kill = mapM_ killThread
