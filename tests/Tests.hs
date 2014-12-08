{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the `humming` executable.
module Main (main) where

import Control.Arrow ((&&&))
import Control.Exception (bracket)
import Control.Monad (when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Data.String (IsString)
import System.Environment (getEnv)
import System.Process (rawSystem, readProcess)
import Test.HUnit (assertEqual, assertFailure)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

-- Unfortuantely the tests require a PostgreSQL server running.
-- TODO Add my Docker-based scripts and make sure my images are available in
-- the public registry.
getConnectionString = do
  host <- getEnv "DB_PORT_5432_TCP_ADDR"
  return $ "dbname=docker user=docker password=docker host=" ++ host

main :: IO ()
main = do
  cs <- getConnectionString
  defaultMain $ tests cs

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
    "Just (1,\"bar\",Array (fromList [Number 1]))\n"
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

doEnqueue connectionString name = do
  _ <- rawSystem "humming"
    [ "enqueue", "--database-url", connectionString
    , "--queue", name, "--method", "bar", "--arguments", "[1]"
    ]
  return ()

doCount connectionString = readProcess "humming"
  [ "count", "--database-url", connectionString
  ] ""

doCountQueue connectionString name = readProcess "humming"
  [ "count", "--database-url", connectionString
  , "--queue", name
  ] ""

doLockQueue connectionString name = readProcess "humming"
  [ "lock", "--database-url", connectionString
  , "--queue", name
  ] ""

deleteAll connectionString = do
  _ <- rawSystem "humming"
    [ "delete", "--database-url", connectionString
    ]
  return ()

deleteOne connectionString i = do
  _ <- rawSystem "humming"
    [ "delete", "--database-url", connectionString
    , "--job", show i
    ]
  return ()

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
