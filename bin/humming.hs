{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Humming.Command qualified as Command
import Humming.Run qualified as Run
import Options.Applicative qualified as A

--------------------------------------------------------------------------------
main :: IO ()
main = A.execParser Command.parserInfo >>= Run.run
