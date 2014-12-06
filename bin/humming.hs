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
    , cmdUnlockDeads
    , cmdListen
    , cmdNotify
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
    { cmdDatabaseUrl :: String
    }
    -- ^ Create the queue_classic_jobs table.
  | Drop
    { cmdDatabaseUrl :: String
    }
    -- ^ Drop the queue_classic_jobs table.
  | Enqueue
    { cmdDatabaseUrl :: String
    , cmdQueueName :: String
    , cmdMethod :: String
    , cmdArguments :: String
    }
    -- ^ Push a job on a queue.
  | Count
    { cmdDatabaseUrl :: String
    , cmdMQueueName :: Maybe String
    }
  | Delete
    { cmdDatabaseUrl :: String
    -- only at most one of --job and --queue can be provided.
    , cmdMQueueName :: Maybe String
    , cmdMJobId :: Maybe Int
    }
    -- ^ Count the jobs on a queue.
  | Lock
    { cmdDatabaseUrl :: String
    , cmdQueueName :: String
    }
    -- ^ Unlock jobs held by dead PostgreSQL processes.
  | UnlockDeads
    { cmdDatabaseUrl :: String
    }
    -- ^ Start to listen for PostgreSQL notifications.
  | Listen
    { cmdDatabaseUrl :: String
    , cmdQueueName :: String
    }
    -- ^ Send a notification through PostgreSQL.
  | Notify
    { cmdDatabaseUrl :: String
    , cmdQueueName :: String
    }
    -- ^ Try to lock a job from a queue.
  | Work
    { cmdDatabaseUrl :: String
    , cmdQueueName :: String
    }
    -- ^ TODO
  deriving (Data, Typeable)

-- | Create a 'Create' command.
cmdCreate :: Cmd
cmdCreate = Create
  { cmdDatabaseUrl = def
    &= explicit
    &= name "database-url"
    &= help "Database URL."
  } &= help "Create the queue_classic_jobs table."
    &= explicit
    &= name "create"

-- | Create a 'Drop' command.
cmdDrop :: Cmd
cmdDrop = Drop
  { cmdDatabaseUrl = def
    &= explicit
    &= name "database-url"
    &= help "Database URL."
  } &= help "Drop the queue_classic_jobs table."
    &= explicit
    &= name "drop"

-- | Create an 'Enqueue' command.
cmdEnqueue :: Cmd
cmdEnqueue = Enqueue
  { cmdDatabaseUrl = def
    &= explicit
    &= name "database-url"
    &= help "Database URL."
  , cmdQueueName = "default"
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
  { cmdDatabaseUrl = def
    &= explicit
    &= name "database-url"
    &= help "Database URL."
  , cmdMQueueName = def
    &= explicit
    &= name "queue"
    &= help "Queue name."
  } &= help "Count the jobs on a queue."
    &= explicit
    &= name "count"

-- | Create a 'Delete' command.
cmdDelete :: Cmd
cmdDelete = Delete
  { cmdDatabaseUrl = def
    &= explicit
    &= name "database-url"
    &= help "Database URL."
  , cmdMQueueName = def
    &= explicit
    &= name "queue"
    &= help "Queue name."
  , cmdMJobId = def
    &= explicit
    &= name "job"
    &= help "Job ID."
  } &= help "Delete a job, a queue, or all jobs."
    &= explicit
    &= name "delete"

-- | Create a 'Lock' command.
cmdLock :: Cmd
cmdLock = Lock
  { cmdDatabaseUrl = def
    &= explicit
    &= name "database-url"
    &= help "Database URL."
  , cmdQueueName = "default"
    &= explicit
    &= name "queue"
    &= help "Queue name."
  } &= help "Try to lock a job from a queue."
    &= explicit
    &= name "lock"

-- | Create an 'UnlockDeads' command.
cmdUnlockDeads :: Cmd
cmdUnlockDeads = UnlockDeads
  { cmdDatabaseUrl = def
    &= explicit
    &= name "database-url"
    &= help "Database URL."
  } &= help "Unlock jobs held by dead PostgreSQL processes."
    &= explicit
    &= name "unlock-deads"

-- | Create a 'Listen' command.
cmdListen :: Cmd
cmdListen = Listen
  { cmdDatabaseUrl = def
    &= explicit
    &= name "database-url"
    &= help "Database URL."
  , cmdQueueName = "default"
    &= explicit
    &= name "queue"
    &= help "Queue name."
  } &= help "Start to listen for PostgreSQL notifications."
    &= explicit
    &= name "listen"

-- | Create a 'Notify' command.
cmdNotify :: Cmd
cmdNotify = Notify
  { cmdDatabaseUrl = def
    &= explicit
    &= name "database-url"
    &= help "Database URL."
  , cmdQueueName = "default"
    &= explicit
    &= name "queue"
    &= help "Queue name."
  } &= help "Send a notification through PostgreSQL."
    &= explicit
    &= name "notify"

-- | Create a 'Work' command.
cmdWork :: Cmd
cmdWork = Work
  { cmdDatabaseUrl = def
    &= explicit
    &= name "database-url"
    &= help "Database URL."
  , cmdQueueName = "default"
    &= explicit
    &= name "queue"
    &= help "Queue name."
  } &= help "A dummy worker; it prints to stdout the job method and arguments."
    &= explicit
    &= name "work"

-- | Run a sub-command.
runCmd :: Cmd -> IO ()
runCmd cmd = do
  con <- connectPostgreSQL . pack $ cmdDatabaseUrl cmd
  _ <- execute_ con "SET application_name='humming'"
  case cmd of
    Create{..} -> do
      Q.create con
    Drop{..} -> do
      Q.drop con
    Enqueue{..} -> do
      case parse json (L.pack cmdArguments) of
        Done _ arguments -> do
          let q = Q.Queue $ L.pack cmdQueueName
          Q.enqueue con q (L.pack cmdMethod) arguments
        _ -> putStrLn "The argument ain't no valid JSON."
    Count{..} -> do
      Q.runCount con (fmap L.pack cmdMQueueName) >>= putStrLn . show
    Delete{..} -> do
      case (cmdMQueueName, cmdMJobId) of
        (Just _, Just _) ->
          putStrLn "Only at most one of --queue and --job can be given."
        (Just queueName, _) -> Q.runDeleteQueue con $ L.pack queueName
        (_, Just jobId) -> Q.runDeleteJob con jobId
        (_, _) -> Q.runDeleteAll con
    Lock{..} -> do
      Q.runLock con (L.pack cmdQueueName) 10 >>= print
    UnlockDeads{..} -> do
      Q.unlockJobsOfDeadWorkers con
    Listen{..} -> do
      Q.listenNotifications con cmdQueueName
    Notify{..} -> do
      Q.sendNotification con cmdQueueName
    Work{..} -> do
      w <- Q.defaultWorker
      Q.start con w { Q.workerQueue = Q.Queue (L.pack cmdQueueName) }
