{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Humming.Command
  ( Command(..)
  , parserInfo
  ) where

import Options.Applicative ((<**>))
import qualified Options.Applicative as A

--------------------------------------------------------------------------------
-- | Data type representing the different command-line subcommands.
data Command =
    Run
    -- ^ Run a temporary PostgreSQL database (using tmp-postgres).
  | Create
    { cmdDatabaseUrl :: String
    , cmdNoScheduling :: Bool
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
    -- ^ Count the jobs on a queue.
  | Delete
    { cmdDatabaseUrl :: String
    -- only at most one of --job and --queue can be provided.
    , cmdMQueueName :: Maybe String
    , cmdMJobId :: Maybe Int
    }
  | Lock
    { cmdDatabaseUrl :: String
    , cmdQueueName :: String
    }
    -- ^ Try to lock a job from a queue.
  | UnlockDeads
    { cmdDatabaseUrl :: String
    }
    -- ^ Unlock jobs held by dead PostgreSQL processes.
  | Listen
    { cmdDatabaseUrl :: String
    , cmdQueueName :: String
    }
    -- ^ Start to listen for PostgreSQL notifications.
  | Notify
    { cmdDatabaseUrl :: String
    , cmdQueueName :: String
    }
    -- ^ Send a notification through PostgreSQL.
  | Work
    { cmdDatabaseUrl :: String
    , cmdQueueName :: String
    , cmdWorkOnce :: Bool
    }
  | Schedule
    { cmdDatabaseUrl :: String
    }
    -- ^ Move scheduled jobs to the queue.
  | Plan
    { cmdDatabaseUrl :: String
    , cmdQueueName :: String
    , cmdMethod :: String
    , cmdArguments :: String
    , cmdSeconds :: Int
    }
    -- ^ Schedule a job (similar to enqueue at a later time).
  deriving (Eq, Show)

--------------------------------------------------------------------------------
parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (cmdParser <**> A.helper) $
    A.fullDesc
      <> A.header versionString
      <> A.progDesc
        "humming - A PostgreSQL-based job queue."

versionString :: String
versionString =
  "humming " {- showVersion version -} ++ " - Copyright (c) 2013-2024 Vo Minh Thu."

--------------------------------------------------------------------------------
cmdParser :: A.Parser Command
cmdParser = A.subparser (
    A.command "run" (A.info (A.helper <*> runParser)
                      (A.progDesc "Create a temporary PostgreSQL database (using tmp-postgres)."))
    <>
    A.command "create" (A.info (A.helper <*> createParser)
                      (A.progDesc "Create the queue_classic_jobs and scheduled_jobs tables."))
    <>
    A.command "drop" (A.info (A.helper <*> dropParser)
                    (A.progDesc "Drop the queue_classic_jobs and scheduled_jobs tables."))
    <>
    A.command "enqueue" (A.info (A.helper <*> enqueueParser)
                       (A.progDesc "Push a job on a queue."))
    <>
    A.command "count" (A.info (A.helper <*> countParser)
                     (A.progDesc "Count the jobs on a queue."))
    <>
    A.command "delete" (A.info (A.helper <*> deleteParser)
                      (A.progDesc "Delete a job, a queue, or all jobs."))
    <>
    A.command "lock" (A.info (A.helper <*> lockParser)
                    (A.progDesc "Try to lock a job from a queue."))
    <>
    A.command "unlock-deads" (A.info (A.helper <*> unlockDeadsParser)
                            (A.progDesc "Unlock jobs held by dead PostgreSQL processes."))
    <>
    A.command "listen" (A.info (A.helper <*> listenParser)
                      (A.progDesc "Start to listen for PostgreSQL notifications."))
    <>
    A.command "notify" (A.info (A.helper <*> notifyParser)
                      (A.progDesc "Send a notification through PostgreSQL."))
    <>
    A.command "work" (A.info (A.helper <*> workParser)
                    (A.progDesc "A dummy worker; it prints to stdout the job method and arguments."))
    <>
    A.command "schedule" (A.info (A.helper <*> scheduleParser)
                        (A.progDesc "Watch the database and move scheduled jobs to the queues."))
    <>
    A.command "plan" (A.info (A.helper <*> planParser)
                    (A.progDesc "Schedule a job in N seconds."))
    )

--------------------------------------------------------------------------------
runParser :: A.Parser Command
runParser = pure Run

createParser :: A.Parser Command
createParser = Create
  <$> parseDatabaseUrl
  <*> A.switch
      ( A.long "no-scheduling"
        <> A.help "Prevent the creation of the scheduled_jobs table." )

dropParser :: A.Parser Command
dropParser = Drop
  <$> parseDatabaseUrl

enqueueParser :: A.Parser Command
enqueueParser = Enqueue
  <$> parseDatabaseUrl
  <*> A.strOption
      ( A.long "queue"
        <> A.metavar "NAME"
        <> A.help "Queue name." )
  <*> A.strOption
      ( A.long "method"
        <> A.metavar "METHOD"
        <> A.help "Method name." )
  <*> A.strOption
      ( A.long "arguments"
        <> A.metavar "JSON"
        <> A.help "Method arguments, in JSON." )

countParser :: A.Parser Command
countParser = Count
  <$> parseDatabaseUrl
  <*> A.optional (A.strOption
      ( A.long "queue"
        <> A.metavar "NAME"
        <> A.help "Queue name." ))

deleteParser :: A.Parser Command
deleteParser = Delete
  <$> parseDatabaseUrl
  <*> A.optional (A.strOption
      ( A.long "queue"
        <> A.metavar "NAME"
        <> A.help "Queue name." ))
  <*> A.optional (A.option A.auto
      ( A.long "job"
        <> A.metavar "ID"
        <> A.help "Job ID." ))

lockParser :: A.Parser Command
lockParser = Lock
  <$> parseDatabaseUrl
  <*> A.strOption
      ( A.long "queue"
        <> A.metavar "NAME"
        <> A.help "Queue name." )

unlockDeadsParser :: A.Parser Command
unlockDeadsParser = UnlockDeads
  <$> parseDatabaseUrl

listenParser :: A.Parser Command
listenParser = Listen
  <$> parseDatabaseUrl
  <*> A.strOption
      ( A.long "queue"
        <> A.metavar "NAME"
        <> A.help "Queue name." )

notifyParser :: A.Parser Command
notifyParser = Notify
  <$> parseDatabaseUrl
  <*> A.strOption
      ( A.long "queue"
        <> A.metavar "NAME"
        <> A.help "Queue name." )

workParser :: A.Parser Command
workParser = Work
  <$> parseDatabaseUrl
  <*> A.strOption
      ( A.long "queue"
        <> A.metavar "NAME"
        <> A.help "Queue name." )
  <*> A.switch
      ( A.long "once"
        <> A.help "Process a single job then exit." )

scheduleParser :: A.Parser Command
scheduleParser = Schedule
  <$> parseDatabaseUrl

planParser :: A.Parser Command
planParser = Plan
  <$> parseDatabaseUrl
  <*> A.strOption
      ( A.long "queue"
        <> A.metavar "NAME"
        <> A.help "Queue name." )
  <*> A.strOption
      ( A.long "method"
        <> A.metavar "METHOD"
        <> A.help "Method name." )
  <*> A.strOption
      ( A.long "arguments"
        <> A.metavar "JSON"
        <> A.help "Method arguments, in JSON." )
  <*> A.option A.auto
      ( A.long "seconds"
        <> A.metavar "SECONDS"
        <> A.help "Number of seconds." )

--------------------------------------------------------------------------------
parseDatabaseUrl :: A.Parser String
parseDatabaseUrl =
  A.strOption
  ( A.long "database-url"
    <> A.metavar "URL"
    <> A.help "Database URL." )
