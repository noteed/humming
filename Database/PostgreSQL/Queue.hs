{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.PostgreSQL.Queue where

import Prelude hiding (catch)

import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Aeson (toJSON, ToJSON, Value)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IORef (IORef, newIORef, readIORef)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import Database.PostgreSQL.Simple.Types (Query(..))

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
  "CREATE TABLE queue_classic_jobs (\n\
  \  id bigserial PRIMARY KEY,\n\
  \  q_name text NOT NULL CHECK(length(q_name) > 0),\n\
  \  method text NOT NULL CHECK(length(method) > 0),\n\
  \  args json NOT NULL,\n\
  \  locked_at timestamptz,\n\
  \  locked_by integer,\n\
  \  created_at timestamptz DEFAULT now()\n\
  \);\n\
  \\n\
  \CREATE INDEX idx_qc_on_name_only_unlocked ON \
  \queue_classic_jobs (q_name, id) WHERE locked_at IS NULL;\n\
  \"

dropTableQuery :: Query
dropTableQuery = "DROP TABLE IF EXISTS queue_classic_jobs"

createFunctionsQuery :: Query
createFunctionsQuery =
-- We are declaring the return type to be queue_classic_jobs.
-- This is ok since I am assuming that all of the users added queues will
-- have identical columns to queue_classic_jobs.
-- When QC supports queues with columns other than the default, we will have to change this.

  "CREATE OR REPLACE FUNCTION lock_head(q_name varchar, top_boundary integer)\n\
  \RETURNS SETOF queue_classic_jobs AS $$\n\
  \DECLARE\n\
  \  unlocked bigint;\n\
  \  relative_top integer;\n\
  \  job_count integer;\n\
  \BEGIN\n\
  \  -- The purpose is to release contention for the first spot in the table.\n\
  \  -- The select count(*) is going to slow down dequeue performance but allow\n\
  \  -- for more workers. Would love to see some optimization here...\n\
  \\n\
  \  EXECUTE 'SELECT count(*) FROM '\n\
  \    || '(SELECT * FROM queue_classic_jobs '\n\
  \    || ' WHERE locked_at IS NULL'\n\
  \    || ' AND q_name = '\n\
  \    || quote_literal(q_name)\n\
  \    || ' LIMIT '\n\
  \    || quote_literal(top_boundary)\n\
  \    || ') limited'\n\
  \  INTO job_count;\n\
  \\n\
  \  SELECT TRUNC(random() * (top_boundary - 1))\n\
  \  INTO relative_top;\n\
  \\n\
  \  IF job_count < top_boundary THEN\n\
  \    relative_top = 0;\n\
  \  END IF;\n\
  \\n\
  \  LOOP\n\
  \    BEGIN\n\
  \      EXECUTE 'SELECT id FROM queue_classic_jobs '\n\
  \        || ' WHERE locked_at IS NULL'\n\
  \        || ' AND q_name = '\n\
  \        || quote_literal(q_name)\n\
  \        || ' ORDER BY id ASC'\n\
  \        || ' LIMIT 1'\n\
  \        || ' OFFSET ' || quote_literal(relative_top)\n\
  \        || ' FOR UPDATE NOWAIT'\n\
  \      INTO unlocked;\n\
  \      EXIT;\n\
  \    EXCEPTION\n\
  \      WHEN lock_not_available THEN\n\
  \        -- do nothing. loop again and hope we get a lock\n\
  \    END;\n\
  \  END LOOP;\n\
  \\n\
  \  RETURN QUERY EXECUTE 'UPDATE queue_classic_jobs '\n\
  \    || ' SET locked_at = (CURRENT_TIMESTAMP),'\n\
  \    || ' locked_by = (SELECT pg_backend_pid())'\n\
  \    || ' WHERE id = $1'\n\
  \    || ' AND locked_at IS NULL'\n\
  \    || ' RETURNING *'\n\
  \  USING unlocked;\n\
  \\n\
  \  RETURN;\n\
  \END;\n\
  \$$ LANGUAGE plpgsql;\n\
  \\n\
  \CREATE OR REPLACE FUNCTION lock_head(tname varchar)\n\
  \RETURNS SETOF queue_classic_jobs AS $$\n\
  \BEGIN\n\
  \  RETURN QUERY EXECUTE 'SELECT * FROM lock_head($1,10)' USING tname;\n\
  \END;\n\
  \$$ LANGUAGE plpgsql;\n\
  \\n\
  \-- queue_classic_notify function and trigger\n\
  \create function queue_classic_notify() returns trigger as $$ begin\n\
  \  perform pg_notify(lower(new.q_name), '');\n\
  \  return null;\n\
  \end;\n\
  \$$ language plpgsql;\n\
  \\n\
  \create trigger queue_classic_notify\n\
  \after insert on queue_classic_jobs\n\
  \for each row\n\
  \execute procedure queue_classic_notify();\n\
  \ "

dropFunctionsQuery :: Query
dropFunctionsQuery =
  "DROP FUNCTION IF EXISTS lock_head(tname varchar);\n\
  \DROP FUNCTION IF EXISTS lock_head(q_name varchar, top_boundary integer);\n\
  \DROP FUNCTION IF EXISTS queue_classic_notify() cascade;"

----------------------------------------------------------------------
-- Queue
----------------------------------------------------------------------

data Queue = Queue
  { queueName :: BC.ByteString
    -- ^ Queue name.
  }

-- | A `NOTIFY` is automatically sent by a trigger.
enqueue :: ToJSON a => Connection -> Queue -> BC.ByteString -> a -> IO ()
enqueue con Queue{..} method args =
  runInsert con queueName method args

runInsert :: ToJSON a =>
  Connection -> BC.ByteString -> BC.ByteString -> a -> IO ()
runInsert con name method args = do
  let q = "INSERT INTO queue_classic_jobs (q_name, method, args) VALUES (?, ?, ?)"
  _ <- execute con q (name, method, toJSON args)
  return ()

toStrict :: L.ByteString -> BC.ByteString
toStrict = BC.concat . L.toChunks

count :: Connection -> Queue -> IO Int
count con Queue{..} = runCount con (Just queueName)

runCount :: Connection -> Maybe BC.ByteString -> IO Int
runCount con mName = do
  let q = "SELECT COUNT(*) FROM queue_classic_jobs"
      q' = "SELECT COUNT(*) FROM queue_classic_jobs \
           \WHERE q_name = ?"
  [Only r] <- case mName of
                Nothing -> query_ con q
                Just name -> query con q' [name]
  return r

delete :: Connection -> Int -> IO ()
delete = runDeleteJob

runDeleteJob :: Connection -> Int -> IO ()
runDeleteJob con i =
  execute con "DELETE FROM queue_classic_jobs where id = ?" [i] >> return ()

deleteAll :: Connection -> Maybe Queue -> IO ()
deleteAll con mqueue = case mqueue of
  Just Queue{..} -> runDeleteQueue con queueName
  Nothing -> runDeleteAll con

runDeleteQueue :: Connection -> BC.ByteString -> IO ()
runDeleteQueue con name = execute con "DELETE FROM queue_classic_jobs\
  \ WHERE q_name = ?" [name] >> return ()

runDeleteAll :: Connection -> IO ()
runDeleteAll con = execute_ con "DELETE FROM queue_classic_jobs" >> return ()

lock :: Connection -> Queue -> Int -> IO (Maybe (Int, BC.ByteString, Value))
lock con Queue{..} topBound = runLock con queueName topBound

runLock :: Connection -> BC.ByteString -> Int -> IO (Maybe (Int, BC.ByteString, Value))
runLock con name topBound = do
  let q = "SELECT id, method, args FROM lock_head(?, ?)"
  rs <- query con q (name, topBound)
  case rs of
    [] -> return Nothing
    [(i, method, arguments)] -> return $ Just (i, method, arguments)
    _ -> error "Must not happen."

-- | Unlock all the jobs for which the PostgreSQL server processes no longer
-- exist to prevent infinitely locked jobs.
unlockJobsOfDeadWorkers :: Connection -> IO ()
unlockJobsOfDeadWorkers con = do
  let q = "UPDATE queue_classic_jobs SET locked_at=NULL, locked_by=NULL \
        \WHERE locked_by NOT IN (SELECT procpid FROM pg_stat_activity)"
  _ <- execute_ con q
  return ()

----------------------------------------------------------------------
-- LISTEN/NOTIFY
-- The NOTIFY is done in a trigger upon job insertion.
----------------------------------------------------------------------

listenNotifications :: Connection -> String -> IO ()
listenNotifications con name = do
  _ <- execute_ con $ Query $ "LISTEN " `BC.append` BC.pack name
  let loop = do
        Notification{..} <- getNotification con
        print (notificationPid, notificationChannel, notificationData)
        loop
  loop

sendNotification :: Connection -> String -> IO ()
sendNotification con name = do
  _ <- execute_ con $ Query $ "NOTIFY " `BC.append` BC.pack name
  return ()

-- TODO Do they really build up ?
drainNotifications :: Connection -> IO ()
drainNotifications con = do
  m <- getNotificationNonBlocking con
  case m of
    Nothing -> return ()
    Just _ -> drainNotifications con

----------------------------------------------------------------------
-- Worker
----------------------------------------------------------------------

data Worker = Worker
  { workerQueue :: Queue
    -- ^ The queue the worker handles.
  , workerTopBound :: Int
  , workerFork :: Bool
    -- ^ Should the worker fork before handling a job ?
  , workerMaxAttempts :: Int
  , workerIsRunning :: IORef Bool
  , workerHandler :: BC.ByteString -> Value -> IO ()
  }

defaultWorker :: IO Worker
defaultWorker = do
  t <- newIORef True
  return $ Worker (Queue "default") 10 False 5 t (curry print)

start :: Connection -> Worker -> IO ()
start con w@Worker{..} = do
  work con w
  continue <- readIORef workerIsRunning
  when continue $ start con w

work :: Connection -> Worker -> IO ()
work con w@Worker{..} = do
  mjob <- lockJob con w
  case mjob of
    Nothing -> return ()
    Just job -> process con w job

lockJob :: Connection -> Worker -> IO (Maybe (Int, BC.ByteString, Value))
lockJob con Worker{..} = do
  let loop = do
        mjob <- lock con workerQueue workerTopBound
        case mjob of
          Just _ -> return mjob
          Nothing -> do
            _ <- execute_ con $ Query $ "LISTEN " `BC.append` queueName workerQueue
            -- TODO The Ruby version waits with a timeout.
            _ <- getNotification con
            _ <- execute_ con $ Query $ "UNLISTEN " `BC.append` queueName workerQueue
            drainNotifications con
            loop
  loop

process :: Connection -> Worker -> (Int, BC.ByteString, Value) -> IO ()
process con w job@(i, method, arguments) =
  catch (call w job) handleException >> delete con i
  where
  handleException :: SomeException -> IO ()
  handleException e = do
    putStrLn $ "An exception occured while processing the job #" ++ show i ++ "."
    putStrLn $ "  - method: " ++ show method
    putStrLn $ "  - argument: " ++ show arguments
    putStrLn $ "  - exception: " ++ show e

call :: Worker -> (Int, BC.ByteString, Value) -> IO ()
call Worker{..} (_, method, arguments) = workerHandler method arguments
