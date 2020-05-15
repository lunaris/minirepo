module Main (main) where

import qualified Codec.Compression.Zlib as Z

main :: IO ()
main =
  print Z.defaultCompression
