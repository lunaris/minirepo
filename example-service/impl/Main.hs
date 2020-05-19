{-# language TemplateHaskell #-}

module Main (main) where

import qualified Codec.Compression.Zlib as Z
import Control.Monad.Reader (runReader)
import qualified Example as Example
import qualified Language.Haskell.TH as TH

main :: IO ()
main = do
  print (runReader Example.doubleEnvironment 21)
  print $(TH.stringE $ show Z.defaultCompression)
  print Z.defaultCompression
