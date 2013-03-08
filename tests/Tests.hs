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
import System.Process (rawSystem, readProcess)
import Test.HUnit (assertEqual, assertFailure)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

databaseUrl = "postgres:///queue_classic_test"

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testCase "Setup/Teardown" $ bracket' $ return ()
  , testCase "Empty/Count" $ bracket' emptyCount
  , testCase "Enqueue/Count" $ bracket' enqueueCount
  , testCase "Empty/Lock" $ bracket' emptyLock
  , testCase "Enqueue/Lock" $ bracket' enqueueLock
  , testCase "Enqueue twice/Delete all" $ bracket' enqueueEnqueueDeleteAll
  , testCase "Enqueue twice/Delete one" $ bracket' enqueueEnqueueDeleteOne
  , testCase "Enqueue twice/Delete queue" $ bracket' enqueueEnqueueDeleteQueue
  ]

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

emptyCount :: IO ()
emptyCount = do
  output <- doCount
  assertEqual "The queue must be empty" "0\n" output

  output <- doCountQueue "FOO"
  assertEqual "The queue must be empty" "0\n" output

enqueueCount :: IO ()
enqueueCount = do
  doEnqueue "FOO"

  output <- doCount
  assertEqual "The queue must contain one job" "1\n" output

  output <- doCountQueue "FOO"
  assertEqual "The queue must contain one job" "1\n" output

emptyLock :: IO ()
emptyLock = do
  output <- doLockQueue "FOO"
  assertEqual "The queue must be empty" "Nothing\n" output

enqueueLock :: IO ()
enqueueLock = do
  doEnqueue "FOO"

  output <- doLockQueue "FOO"
  assertEqual "A job must have been locked"
    "Just (1,Chunk \"bar\" Empty,Array (fromList [Number 1]))\n"
    output

enqueueEnqueueDeleteAll :: IO ()
enqueueEnqueueDeleteAll = do
  doEnqueue "FOO"
  doEnqueue "FOO"

  output <- doCount
  assertEqual "The queue must contain two jobs" "2\n" output

  output <- doCountQueue "FOO"
  assertEqual "The queue must contain two jobs" "2\n" output

  deleteAll

  output <- doCount
  assertEqual "The queue must be empty" "0\n" output

  output <- doCountQueue "FOO"
  assertEqual "The queue must be empty" "0\n" output

enqueueEnqueueDeleteOne :: IO ()
enqueueEnqueueDeleteOne = do
  doEnqueue "FOO"
  doEnqueue "FOO"

  output <- doCount
  assertEqual "The queue must contain two jobs" "2\n" output

  output <- doCountQueue "FOO"
  assertEqual "The queue must contain two jobs" "2\n" output

  deleteOne 1

  output <- doCount
  assertEqual "The queue must contain one job" "1\n" output

  output <- doCountQueue "FOO"
  assertEqual "The queue must contain one job" "1\n" output

enqueueEnqueueDeleteQueue :: IO ()
enqueueEnqueueDeleteQueue = do
  doEnqueue "FOO"
  doEnqueue "BAR"

  output <- doCount
  assertEqual "The queue must contain two jobs" "2\n" output

  output <- doCountQueue "FOO"
  assertEqual "The queue must contain one job" "1\n" output

  output <- doCountQueue "BAR"
  assertEqual "The queue must contain one job" "1\n" output

  deleteQueue "FOO"

  output <- doCount
  assertEqual "The queue must contain one job" "1\n" output

  output <- doCountQueue "FOO"
  assertEqual "The queue must be empty" "0\n" output

  output <- doCountQueue "BAR"
  assertEqual "The queue must contain one job" "1\n" output

----------------------------------------------------------------------
-- Command-line wrappers
----------------------------------------------------------------------

doEnqueue name = do
  _ <- rawSystem "humming"
    [ "enqueue", "--database-url", databaseUrl
    , "--queue", name, "--method", "bar", "--arguments", "[1]"
    ]
  return ()

doCount = readProcess "humming"
  [ "count", "--database-url", databaseUrl
  ] ""

doCountQueue name = readProcess "humming"
  [ "count", "--database-url", databaseUrl
  , "--queue", name
  ] ""

doLockQueue name = readProcess "humming"
  [ "lock", "--database-url", databaseUrl
  , "--queue", name
  ] ""

deleteAll = do
  _ <- rawSystem "humming"
    [ "delete", "--database-url", databaseUrl
    ]
  return ()

deleteOne i = do
  _ <- rawSystem "humming"
    [ "delete", "--database-url", databaseUrl
    , "--job", show i
    ]
  return ()

deleteQueue name = do
  _ <- rawSystem "humming"
    [ "delete", "--database-url", databaseUrl
    , "--queue", name
    ]
  return ()

----------------------------------------------------------------------
-- Setup / Teardown
----------------------------------------------------------------------

setup :: IO ()
setup = do
  _ <-rawSystem "humming" ["create", "--database-url", databaseUrl]
  return ()

teardown :: () -> IO ()
teardown _ = do
  _ <-rawSystem "humming" ["drop", "--database-url", databaseUrl]
  return ()

bracket' :: IO () -> IO ()
bracket' = bracket setup teardown . const
