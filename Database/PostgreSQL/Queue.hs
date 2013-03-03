{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.Queue where

import Database.PostgreSQL.Simple

----------------------------------------------------------------------
-- Setup
----------------------------------------------------------------------

create :: Connection -> IO ()
create con = createTable con >> createFunctions con

drop :: Connection -> IO ()
drop con = dropTable con >> dropFunctions con

createTable :: Connection -> IO ()
createTable con = withTransaction con $
  execute_ con createTableQuery >> return ()

dropTable :: Connection -> IO ()
dropTable con = withTransaction con $
  execute_ con dropTableQuery >> return ()

createFunctions :: Connection -> IO ()
createFunctions con = return () -- TODO

dropFunctions :: Connection -> IO ()
dropFunctions con = return () -- TODO

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
