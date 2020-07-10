{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}

module Main (main) where

import qualified Codec.Compression.Zlib as Z
import Control.Monad.Reader (runReader)
import qualified Database.PostgreSQL.Simple as PG
import Data.String (fromString)
import qualified Example as Example
import qualified Language.Haskell.TH as TH
import qualified System.Environment as Env

-- |Our application's entry point.
main :: IO ()
main = do
  --  Test some code that comes from an in-repository dependency built with
  --  Bazel.
  print (runReader Example.doubleEnvironment 21)

  --  Test Template Haskell that loads code that comes from an in-repository
  --  dependency built with Bazel.
  putStrLn $(TH.stringE $ Example.someString)

  --  Test some code that comes from a Hackage dependency built with Cabal
  --  (through Bazel). In particular, we target a library that has C
  --  dependencies (`zlib` in this case).
  print Z.defaultCompression

  --  Test Template Haskell that loads code that comes from a Hackage dependency
  --  built with Cabal (through Bazel).
  putStrLn $(TH.stringE $ show Z.defaultCompression)

  --  A slightly chunkier test of some code that comes from a Hackage dependency
  --  built with Cabal (through Bazel) and that more heavily exercises C code
  --  (here, `libpq`, which is used indirectly by `postgresql-simple`).
  maybeConnStr <- Env.lookupEnv "PG_CONNECTION_STRING"
  case maybeConnStr of
    Just connStr -> do
      putStrLn "Testing PostgreSQL"
      conn <- PG.connectPostgreSQL (fromString connStr)
      [PG.Only (x :: Int)] <- PG.query_ conn "select 1 + 1"
      print x

    Nothing ->
      putStrLn "No connection string given, no PostgreSQL testing performed"
