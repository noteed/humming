{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.PostgreSQL.Queue where

import Prelude hiding (catch)

import Control.Exception (SomeException, catch)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Aeson (decode, encode, Object, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IORef (IORef, newIORef, readIORef)
import Database.PostgreSQL.Simple

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
  \  q_name varchar(255),\n\
  \  method varchar(255),\n\
  \  args text,\n\
  \  locked_at timestamptz\n\
  \);\n\
  \\n\
  \CREATE INDEX idx_qc_on_name_only_unlocked ON \
  \queue_classic_jobs (q_name, id) WHERE locked_at IS NULL;"

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
  \    || '(SELECT * FROM queue_classic_jobs WHERE q_name = '\n\
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
  \    || ' SET locked_at = (CURRENT_TIMESTAMP)'\n\
  \    || ' WHERE id = $1'\n\
  \    || ' AND locked_at is NULL'\n\
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
  \$$ LANGUAGE plpgsql;"

dropFunctionsQuery :: Query
dropFunctionsQuery =
  "DROP FUNCTION IF EXISTS lock_head(tname varchar);\n\
  \DROP FUNCTION IF EXISTS lock_head(q_name varchar, top_boundary integer)"

----------------------------------------------------------------------
-- Queue
----------------------------------------------------------------------

data Queue = Queue
  { queueName :: L.ByteString
    -- ^ Queue name.
  , queueChannel :: Maybe L.ByteString
    -- ^ LISTEN channel for this queue.
  }

enqueue :: ToJSON a => Connection -> Queue -> L.ByteString -> a -> IO ()
enqueue con Queue{..} method args =
  runInsert con queueName method args queueChannel

runInsert :: ToJSON a =>
  Connection -> L.ByteString -> L.ByteString -> a -> Maybe L.ByteString -> IO ()
runInsert con name method args _ = do
  let q = "INSERT INTO queue_classic_jobs (q_name, method, args) VALUES (?, ?, ?)"
  _ <- execute con q [name, method, encode args]
  -- TODO LISTEN/NOTIFY support
  -- notify chan
  return ()

count :: Connection -> Queue -> IO Int
count con Queue{..} = runCount con (Just queueName)

runCount :: Connection -> Maybe L.ByteString -> IO Int
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

runDeleteQueue :: Connection -> L.ByteString -> IO ()
runDeleteQueue con name = execute con "DELETE FROM queue_classic_jobs\
  \ WHERE q_name = ?" [name] >> return ()

runDeleteAll :: Connection -> IO ()
runDeleteAll con = execute_ con "DELETE FROM queue_classic_jobs" >> return ()

lock :: Connection -> Queue -> Int -> IO (Maybe (Int, L.ByteString, Object))
lock con Queue{..} topBound = runLock con queueName topBound

runLock :: Connection -> L.ByteString -> Int -> IO (Maybe (Int, L.ByteString, Object))
runLock con name topBound = do
  let q = "SELECT id, method, args FROM lock_head(?, ?)"
  rs <- query con q (name, topBound)
  case rs of
    [] -> return Nothing
    [(i, method, arguments)] ->
      case decode arguments of
        Nothing -> putStrLn "Can't decode arguments" >> return Nothing --TODO
        Just as ->  return $ Just (i, method, as)
    _ -> error "Must not happen."

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
  , workerHandler :: L.ByteString -> Object -> IO ()
  }

defaultWorker :: IO Worker
defaultWorker = do
  t <- newIORef True
  return $ Worker (Queue (L.pack "default") Nothing) 10 False 5 t (curry print)

start :: Connection -> Worker -> IO ()
start con w@Worker{..} = do
  work con w
  continue <- readIORef workerIsRunning
  when continue $ start con w

work :: Connection -> Worker -> IO ()
work con w@Worker{..} = do
  mjob <- lockJob con w 0
  case mjob of
    Nothing -> return ()
    Just job -> process con w job

lockJob :: Connection -> Worker -> Int -> IO (Maybe (Int, L.ByteString, Object))
lockJob con w@Worker{..} attempt = do
  mjob <- lock con workerQueue workerTopBound
  case mjob of
    Just _ -> return mjob
    Nothing -> do
      if attempt < workerMaxAttempts
        then do
          putStrLn $ "Attempt #" ++ show attempt
          threadDelay ((2 ^ attempt) * 1000000)
          lockJob con w $ succ attempt
        else return Nothing

process :: Connection -> Worker -> (Int, L.ByteString, Object) -> IO ()
process con w job@(i, method, arguments) =
  catch (call w job) handleException >> delete con i
  where
  handleException :: SomeException -> IO ()
  handleException e = do
    putStrLn $ "An exception occured while processing the job #" ++ show i ++ "."
    putStrLn $ "  - method: " ++ show method
    putStrLn $ "  - argument: " ++ show arguments
    putStrLn $ "  - exception: " ++ show e

call :: Worker -> (Int, L.ByteString, Object) -> IO ()
call Worker{..} (_, method, arguments) = workerHandler method arguments
