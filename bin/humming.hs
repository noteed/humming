{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Aeson (json)
import Data.Attoparsec.Lazy (parse, Result(..))
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Version (showVersion)
import Database.PostgreSQL.Simple
import Paths_humming (version)
import System.Console.CmdArgs.Implicit
import System.Environment (getEnvironment)

import qualified Database.PostgreSQL.Queue as Q

main :: IO ()
main = (runCmd =<<) $ cmdArgs $
  modes
    [ cmdCreate
    , cmdDrop
    , cmdEnqueue
    , cmdCount
    , cmdDelete
    , cmdLock
    , cmdWork
    ]
  &= summary versionString
  &= program "humming"

-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "humming " ++ showVersion version ++ " - Copyright (c) 2013 Vo Minh Thu."

-- | Data type representing the different command-line subcommands.
data Cmd =
    Create
    -- ^ Create the queue_classic table.
  | Drop
    -- ^ Drop the queue_classic table.
  | Enqueue
    { cmdQueueName :: String
    , cmdMethod :: String
    , cmdArguments :: String
    }
    -- ^ Push a job on a queue.
  | Count
    { cmdMQueueName :: Maybe String
    }
  | Delete
    { cmdJobId :: Int
    }
    -- ^ Count the jobs on a queue.
  | Lock
    { cmdQueueName :: String
    }
    -- ^ Try to lock a job from a queue.
  | Work
    { cmdQueueName :: String
    }
    -- ^ TODO
  deriving (Data, Typeable)

-- | Create a 'Create' command.
cmdCreate :: Cmd
cmdCreate = Create
    &= help "Create the queue_classic_jobs table."
    &= explicit
    &= name "create"

-- | Create a 'Drop' command.
cmdDrop :: Cmd
cmdDrop = Drop
    &= help "Drop the queue_classic_jobs table."
    &= explicit
    &= name "drop"

-- | Create an 'Enqueue' command.
cmdEnqueue :: Cmd
cmdEnqueue = Enqueue
  { cmdQueueName = "default"
    &= explicit
    &= name "queue"
    &= help "Queue name."
  , cmdMethod = def
    &= explicit
    &= name "method"
    &= help "Method name."
  , cmdArguments = def
    &= typ "JSON"
    &= explicit
    &= name "arguments"
    &= help "Method arguments, in JSON."
  } &= help "Push a job on a queue."
    &= explicit
    &= name "enqueue"

-- | Create a 'Count' command.
cmdCount :: Cmd
cmdCount = Count
  { cmdMQueueName = def
    &= explicit
    &= name "queue"
    &= help "Queue name."
  } &= help "Count the jobs on a queue."
    &= explicit
    &= name "count"

-- | Create a 'Delete' command.
cmdDelete :: Cmd
cmdDelete = Delete
  { cmdJobId = def
    &= explicit
    &= name "job"
    &= help "Job ID."
  } &= help "Delete a job."
    &= explicit
    &= name "delete"

-- | Create a 'Lock' command.
cmdLock :: Cmd
cmdLock = Lock
  { cmdQueueName = "default"
    &= explicit
    &= name "queue"
    &= help "Queue name."
  } &= help "Try to lock a job from a queue."
    &= explicit
    &= name "lock"

-- | Create a 'Work' command.
cmdWork :: Cmd
cmdWork = Work
  { cmdQueueName = "default"
    &= explicit
    &= name "queue"
    &= help "Queue name."
  } &= help "A dummy worker; it prints to stdout the job method and arguments."
    &= explicit
    &= name "work"

-- | Run a sub-command.
runCmd :: Cmd -> IO ()
runCmd cmd = do
  env <- getEnvironment
  let mConString = lookup "QC_DATABASE_URL" env
      mConString' = maybe (lookup "DATABASE_URL" env) Just mConString
  case mConString' of
    Nothing -> putStrLn $ "The QC_DATABASE_URL or DATABASE_URL "
      ++ "environment variable must be defined."

    Just connectionString -> case cmd of
      Create{..} -> do
        con <- connectPostgreSQL $ pack connectionString
        Q.create con
      Drop{..} -> do
        con <- connectPostgreSQL $ pack connectionString
        Q.drop con
      Enqueue{..} -> do
        case parse json (L.pack cmdArguments) of
          Done _ arguments -> do
            con <- connectPostgreSQL $ pack connectionString
            let q = Q.Queue (L.pack cmdQueueName) Nothing
            Q.enqueue con q (L.pack cmdMethod) arguments
          _ -> putStrLn "The argument ain't no valid JSON."
      Count{..} -> do
        con <- connectPostgreSQL $ pack connectionString
        Q.runCount con (fmap L.pack cmdMQueueName) >>= putStrLn . show
      Delete{..} -> do
        con <- connectPostgreSQL $ pack connectionString
        Q.runDelete con cmdJobId
      Lock{..} -> do
        con <- connectPostgreSQL $ pack connectionString
        Q.runLock con (L.pack cmdQueueName) 10 >>= print
      Work{..} -> do
        con <- connectPostgreSQL $ pack connectionString
        w <- Q.defaultWorker
        Q.start con w { Q.workerQueue = Q.Queue (L.pack cmdQueueName) Nothing }
