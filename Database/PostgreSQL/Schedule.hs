{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.PostgreSQL.Schedule where

import Control.Concurrent (forkIO)
import Data.Aeson (encode, ToJSON)
import Data.AffineSpace ((.-.))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Thyme.Clock (fromSeconds, getCurrentTime, toSeconds, UTCTime)
import Data.Thyme.Time (fromGregorian, mkUTCTime, utcTimeToPOSIXSeconds)
import qualified Data.Time.Clock as C (UTCTime)
import qualified Data.Time.Clock.POSIX as C (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Thyme.Format (formatTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification

import System.IO (stdout, hFlush)
import System.Locale (defaultTimeLocale, iso8601DateFormat)

import System.Cron.WakeUp

import qualified Database.PostgreSQL.Queue as Q

----------------------------------------------------------------------
-- Setup
----------------------------------------------------------------------

-- All the SQL queries are the ones from queue_classic.

create :: Connection -> IO ()
create con = createTable con >> createFunctions con

drop :: Connection -> IO ()
drop con = dropFunctions con >> dropTable con

createTable :: Connection -> IO ()
createTable con = withTransaction con $
  execute_ con createTableQuery >> return ()

dropTable :: Connection -> IO ()
dropTable con = withTransaction con $
  execute_ con dropTableQuery >> return ()

createFunctions :: Connection -> IO ()
createFunctions con = withTransaction con $
  execute_ con createFunctionsQuery >> return ()

dropFunctions :: Connection -> IO ()
dropFunctions con = withTransaction con $
  execute_ con dropFunctionsQuery >> return ()

----------------------------------------------------------------------
-- Setup query strings
----------------------------------------------------------------------

createTableQuery :: Query
createTableQuery =
  "CREATE TABLE scheduled_jobs (\n\
  \  id bigserial PRIMARY KEY,\n\
  \  q_name text NOT NULL CHECK(length(q_name) > 0),\n\
  \  method text NOT NULL CHECK(length(method) > 0),\n\
  \  args text NOT NULL,\n\
  \  created_at timestamptz DEFAULT now(),\n\
  \  next_push_at timestamptz\n\
  \);\n\
  \ "

dropTableQuery :: Query
dropTableQuery = "DROP TABLE IF EXISTS scheduled_jobs"

createFunctionsQuery :: Query
createFunctionsQuery =
  "-- humming_schedule_notify function and trigger\n\
  \CREATE FUNCTIOn humming_schedule_notify()\n\
  \RETURNS TRIGGER AS $$\n\
  \BEGIN\n\
  \  PERFORM pg_notify('humming_schedule', '');\n\
  \  RETURN NULL;\n\
  \END;\n\
  \$$ LANGUAGE plpgsql;\n\
  \\n\
  \CREATE TRIGGER humming_schedule_notify\n\
  \AFTER UPDATE OR INSERT OR DELETE ON scheduled_jobs\n\
  \FOR EACH ROW\n\
  \EXECUTE PROCEDURE humming_schedule_notify();\n\
  \ "

dropFunctionsQuery :: Query
dropFunctionsQuery =
  "DROP FUNCTION IF EXISTS humming_schedule_notify() cascade;"

nextScheduledJob :: Connection -> IO (Maybe Task)
nextScheduledJob con = do
  jobs <- query_ con
    "SELECT id, next_push_at, q_name, method, args FROM scheduled_jobs \
    \ORDER BY next_push_at ASC LIMIT 1"
  case jobs of
    [] -> return Nothing
    -- Use Data.Time.Clock ...
    (i, r::C.UTCTime, q, m, a):_ -> do
      -- ... then convert from epoch to Thyme's UTCTime.
      let secondsSinceEpoch = (floor $ C.utcTimeToPOSIXSeconds r) :: Int
          t = mkUTCTime (fromGregorian 1970 0 0) (fromSeconds secondsSinceEpoch)
      return . Just $ Task i t q m a Nothing
      -- TODO Task repetition.

----------------------------------------------------------------------
-- Scheduled jobs
----------------------------------------------------------------------

-- | A `NOTIFY` is automatically sent by a trigger.
plan :: ToJSON a => Connection -> Text -> Text -> a -> UTCTime -> IO ()
plan con name method args at =
  runInsert con name method args at

runInsert :: ToJSON a =>
  Connection -> Text -> Text -> a -> UTCTime -> IO ()
runInsert con name method args at = do
  let q = "INSERT INTO scheduled_jobs (q_name, method, args, next_push_at) VALUES (?, ?, ?, ?)"
  _ <- execute con q (name, method, Q.toStrict $ encode args, at')
  return ()
  where
  at' = C.posixSecondsToUTCTime $ toSeconds $ utcTimeToPOSIXSeconds at

----------------------------------------------------------------------
-- Job scheduling
----------------------------------------------------------------------

-- | Observe the database and move jobs from scheduled_jobs to
-- queue_classic_jobs.
schedule :: Connection -> IO ()
schedule con = wakeupService True $ \request -> do

  -- Thread to detect a change in the set of jobs.
  let loop = do
        mjob <- nextScheduledJob con
        case mjob of
          Nothing -> return ()
          Just job -> do
            putStrLn "The set of scheduled jobs has changed."
            hFlush stdout
            amount <- amountToSleep job
            request amount

        _ <- execute_ con "LISTEN humming_schedule"
        -- TODO The Ruby version waits with a timeout.
        _ <- getNotification con
        _ <- execute_ con "UNLISTEN humming_schedule"
        Q.drainNotifications con
        loop

  _ <- forkIO loop

  return . Client $ runTasks con

-- | Give a chance to tasks to be run. Return possibly a request to receive
-- another wakeup n seconds later.
runTasks :: Connection -> IO (Maybe Int)
runTasks con = do
  mtask <- nextScheduledJob con
  case mtask of
    Nothing -> return Nothing
    Just task -> do
      amount <- amountToSleep task
      if amount > 0
        then do
          -- Wakeup is too early.
          putStrLn $ "Next task: " ++ T.unpack (taskMethod task) ++ " @ " ++
            formatTime locale format (taskWhen task)
          hFlush stdout
          return $ Just amount
        else do
          putStrLn $ "Running: " ++ T.unpack (taskMethod task) ++ " @ " ++
            formatTime locale format (taskWhen task)
          hFlush stdout

          _ <- execute con
            "INSERT INTO queue_classic_jobs (q_name, method, args) VALUES (?, ?, ?)"
            [taskQueueName task, taskMethod task, taskArguments task]

          -- Remove this task.
          _ <- execute con
            "DELETE FROM scheduled_jobs WHERE ID=?" [taskId task]

          -- Select next task and return how long to wait.
          mtask' <- nextScheduledJob con
          case mtask' of
            Nothing -> return Nothing
            Just task' -> do
              amount' <- amountToSleep task'
              putStrLn $ "Next task: " ++ T.unpack (taskMethod task') ++ " @ " ++
                formatTime locale format (taskWhen task')
              hFlush stdout
              return $ Just amount'
  where
  locale = defaultTimeLocale
  format = iso8601DateFormat $ Just "%H:%M:%S"

amountToSleep :: Task -> IO Int
amountToSleep Task{..} = do
  now <- getCurrentTime
  -- TODO In a recent version of Thyme, we could use `microseconds` instead
  -- of second * 10^6.
  return $ ceiling . (* (1000 :: Double)) . (* 1000) . toSeconds $ taskWhen .-. now

data Task = Task
  { taskId :: Int
  , taskWhen :: UTCTime
  , taskQueueName :: Text
  , taskMethod :: Text
  , taskArguments :: Text
  -- ^ This is JSON, but we don't decode/re-encode (as we read from
  -- scheduled_jobs to write directly to queue_classic_jobs).
  , taskRepetition :: Maybe (Maybe Int, Int)
  -- ^ Possibly repeat the task, possibly a finite number of times, every n
  -- seconds.
  }
