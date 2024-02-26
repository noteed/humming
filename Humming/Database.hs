module Humming.Database where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as B
import Data.Pool (Pool, createPool)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Postgres.Temp as PGTemp
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)

--------------------------------------------------------------------------------

-- | Run a temporary PostgreSQL database (using tmp-postgres), and display its
-- connection string. This then waits for a ctrl-c to dispose of the database.
waitTemporaryDb :: Bool -> IO ()
waitTemporaryDb quiet = do
  withTemporaryConnection quiet $ \_ -> do
    unless quiet $ putStrLn "Type ctrl-c to shutdown."
    sleepForever

withTemporaryConnection :: Bool -> (Pool PG.Connection -> IO a) -> IO a
withTemporaryConnection quiet f =
  -- This creates a ~/.tmp-postgres directory.
  PGTemp.withDbCache $ \cache -> do
    merr <- PGTemp.withConfig (PGTemp.cacheConfig cache) $ \db -> do
      let connstr = PGTemp.toConnectionString db
      if quiet
        then do
          putStrLn $ B.unpack connstr
          hFlush stdout
        else putStrLn $ "Connection string: " <> show connstr
      withConnectionString connstr $ \pool -> do
        f pool
    case merr of
      Right a -> pure a
      Left err -> do
        putStrLn $ "Can't create temporary database: " <> show err
        exitFailure

withConnectionString :: B.ByteString -> (Pool PG.Connection -> IO a) -> IO a
withConnectionString connstr f = do
  let connect = PG.connectPostgreSQL connstr
  pool <- createPool connect PG.close 1 3600 64
  f pool

sleepForever :: IO ()
sleepForever = go
 where
  go = threadDelay (3600 * 1000000) >> go
