{-# language FlexibleContexts #-}

module Example where

import Control.Monad.Reader (MonadReader (..))

-- |Just some string.
someString :: String
someString = "Hello, world, from a library!"

-- |A bit more interesting than 'someString'. You can treat it as a plain-old
--  function if you use the '(->)' instance of 'MonadReader'.
doubleEnvironment :: MonadReader Int m => m Int
doubleEnvironment = do
  x <- ask
  pure (x + x)
