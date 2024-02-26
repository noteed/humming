{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- | Tests for the `humming` executable.
module Humming.Runner
  ( CliOrFun(..)
  , runTests
  ) where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BC
import qualified Database.Postgres.Temp as PGTemp
import System.Process (rawSystem, readProcess)
import Test.HUnit (assertEqual)
import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase)

-- Use the humming binary, or use the underlying functions. When building the
-- package with Nix, the binary is not available (since we're building it).
-- When using ghci-with-database.sh, it it available and can be used in these
-- tests.

data CliOrFun = Cli | Fun
  deriving (Eq, Show)

-------------------------------------------------------------------------------
runTests :: CliOrFun -> IO ()
runTests Cli = do
  merr <- PGTemp.with $ \db -> do
    let connstr = PGTemp.toConnectionString db
    defaultMain $ tests $ BC.unpack connstr
  case merr of
    Left err -> error $ show err
    Right () -> pure ()

-- TODO.
runTests Fun = pure ()

tests :: String -> [Test]
tests connectionString =
  [ testCase "Setup/Teardown" $ bracket' connectionString $ const (return ())
  , testCase "Empty/Count" $ bracket' connectionString emptyCount
  , testCase "Enqueue/Count" $ bracket' connectionString enqueueCount
  , testCase "Empty/Lock" $ bracket' connectionString emptyLock
  , testCase "Enqueue/Lock" $ bracket' connectionString enqueueLock
  , testCase "Enqueue twice/Delete all" $ bracket' connectionString enqueueEnqueueDeleteAll
  , testCase "Enqueue twice/Delete one" $ bracket' connectionString enqueueEnqueueDeleteOne
  , testCase "Enqueue twice/Delete queue" $ bracket' connectionString enqueueEnqueueDeleteQueue
  ]

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

emptyCount :: String -> IO ()
emptyCount connectionString = do
  output <- doCount connectionString
  assertEqual "The queue must be empty" "0\n" output

  output <- doCountQueue connectionString "FOO"
  assertEqual "The queue must be empty" "0\n" output

enqueueCount :: String -> IO ()
enqueueCount connectionString = do
  doEnqueue connectionString "FOO"

  output <- doCount connectionString
  assertEqual "The queue must contain one job" "1\n" output

  output <- doCountQueue connectionString "FOO"
  assertEqual "The queue must contain one job" "1\n" output

emptyLock :: String -> IO ()
emptyLock connectionString = do
  output <- doLockQueue connectionString "FOO"
  assertEqual "The queue must be empty" "Nothing\n" output

enqueueLock :: String -> IO ()
enqueueLock connectionString = do
  doEnqueue connectionString "FOO"

  output <- doLockQueue connectionString "FOO"
  assertEqual "A job must have been locked"
    "Just (1,\"bar\",Array [Number 1.0])\n"
    output

enqueueEnqueueDeleteAll :: String -> IO ()
enqueueEnqueueDeleteAll connectionString = do
  doEnqueue connectionString "FOO"
  doEnqueue connectionString "FOO"

  output <- doCount connectionString
  assertEqual "The queue must contain two jobs" "2\n" output

  output <- doCountQueue connectionString "FOO"
  assertEqual "The queue must contain two jobs" "2\n" output

  deleteAll connectionString

  output <- doCount connectionString
  assertEqual "The queue must be empty" "0\n" output

  output <- doCountQueue connectionString "FOO"
  assertEqual "The queue must be empty" "0\n" output

enqueueEnqueueDeleteOne :: String -> IO ()
enqueueEnqueueDeleteOne connectionString = do
  doEnqueue connectionString "FOO"
  doEnqueue connectionString "FOO"

  output <- doCount connectionString
  assertEqual "The queue must contain two jobs" "2\n" output

  output <- doCountQueue connectionString "FOO"
  assertEqual "The queue must contain two jobs" "2\n" output

  deleteOne connectionString 1

  output <- doCount connectionString
  assertEqual "The queue must contain one job" "1\n" output

  output <- doCountQueue connectionString "FOO"
  assertEqual "The queue must contain one job" "1\n" output

enqueueEnqueueDeleteQueue :: String -> IO ()
enqueueEnqueueDeleteQueue connectionString = do
  doEnqueue connectionString "FOO"
  doEnqueue connectionString "BAR"

  output <- doCount connectionString
  assertEqual "The queue must contain two jobs" "2\n" output

  output <- doCountQueue connectionString "FOO"
  assertEqual "The queue must contain one job" "1\n" output

  output <- doCountQueue connectionString "BAR"
  assertEqual "The queue must contain one job" "1\n" output

  deleteQueue connectionString "FOO"

  output <- doCount connectionString
  assertEqual "The queue must contain one job" "1\n" output

  output <- doCountQueue connectionString "FOO"
  assertEqual "The queue must be empty" "0\n" output

  output <- doCountQueue connectionString "BAR"
  assertEqual "The queue must contain one job" "1\n" output

----------------------------------------------------------------------
-- Command-line wrappers
----------------------------------------------------------------------

doEnqueue :: String -> String -> IO ()
doEnqueue connectionString name = do
  _ <- rawSystem "humming"
    [ "enqueue", "--database-url", connectionString
    , "--queue", name, "--method", "bar", "--arguments", "[1]"
    ]
  return ()

doCount :: String -> IO String
doCount connectionString = readProcess "humming"
  [ "count", "--database-url", connectionString
  ] ""

doCountQueue :: String -> String -> IO String
doCountQueue connectionString name = readProcess "humming"
  [ "count", "--database-url", connectionString
  , "--queue", name
  ] ""

doLockQueue :: String -> String -> IO String
doLockQueue connectionString name = readProcess "humming"
  [ "lock", "--database-url", connectionString
  , "--queue", name
  ] ""

deleteAll :: String -> IO ()
deleteAll connectionString = do
  _ <- rawSystem "humming"
    [ "delete", "--database-url", connectionString
    ]
  return ()

deleteOne :: String -> Int -> IO ()
deleteOne connectionString i = do
  _ <- rawSystem "humming"
    [ "delete", "--database-url", connectionString
    , "--job", show i
    ]
  return ()

deleteQueue :: String -> String -> IO ()
deleteQueue connectionString name = do
  _ <- rawSystem "humming"
    [ "delete", "--database-url", connectionString
    , "--queue", name
    ]
  return ()

----------------------------------------------------------------------
-- Setup / Teardown
----------------------------------------------------------------------

setup :: String -> IO ()
setup connectionString = do
  _ <-rawSystem "humming" ["create", "--database-url", connectionString]
  return ()

teardown :: String -> IO ()
teardown connectionString = do
  _ <-rawSystem "humming" ["drop", "--database-url", connectionString]
  return ()

bracket' :: String -> (String -> IO ()) -> IO ()
bracket' connectionString = bracket (setup connectionString >> return connectionString) teardown
