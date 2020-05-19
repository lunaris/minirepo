{-# language FlexibleContexts #-}

module Example where

import Control.Monad.Reader (MonadReader (..))

someString :: String
someString = "Hello, world, from a library!"

doubleEnvironment :: MonadReader Int m => m Int
doubleEnvironment = do
  x <- ask
  pure (x + x)
