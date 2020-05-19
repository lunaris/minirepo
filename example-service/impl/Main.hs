{-# language TemplateHaskell #-}

module Main (main) where

import qualified Codec.Compression.Zlib as Z
import Control.Monad.Reader (runReader)
import qualified Database.PostgreSQL.Simple as PG
import qualified Example as Example
import qualified Language.Haskell.TH as TH
import qualified System.Environment as Env

main :: IO ()
main = do
  print (runReader Example.doubleEnvironment 21)
  print $(TH.stringE $ show Z.defaultCompression)
  print Z.defaultCompression

  maybeConnStr <- Env.lookupEnv "PG_CONNECTION_STRING"
  case maybeConnStr of
    Just connStr -> do
      conn <- PG.connectPostgreSQL connStr
      [PG.Only x] <- PG.query "select 1"
      print x

    Nothing ->
      putStrLn "No connection string given, no PostgreSQL testing performed"
