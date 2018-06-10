module Main
  ( main
  )
where

import qualified Beeswax
import qualified Data.ByteString as Bytes
import qualified System.Environment as Environment
import qualified System.IO as IO

main :: IO ()
main = do
  IO.hPrint IO.stderr Beeswax.version
  [input, output] <- Environment.getArgs
  contents <- Bytes.readFile input
  case Beeswax.decodeObject contents of
    Left problem -> fail problem
    Right object -> Bytes.writeFile output (Beeswax.encodeObject object)
