{-# LANGUAGE BangPatterns #-}

module Data.Conduit.Utils where


-------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Trans
import           Data.Conduit
-------------------------------------------------------------------------------



-- | Perform a given monadic action once every X elements flowing
-- through this conduit.
performEvery
    :: (Monad m)
    => Integer
    -> (Integer -> m ())
    -> ConduitM a a m ()
performEvery n f = go 0
    where
      go !i = do
          x <- await
          case x of
            Nothing -> return ()
            Just x' -> do
                when (i `mod` n == 0) $ lift (f i)
                yield x'
                go $! i + 1
