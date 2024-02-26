{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Humming.Run
  ( run
  ) where

import qualified Humming.Command as Command

import Control.Monad (when)
import Data.Aeson (json)
import Data.AffineSpace ((.+^))
import Data.Attoparsec.Lazy (parse, Result(..))
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Data.Thyme.Clock (fromSeconds, getCurrentTime)
import Database.PostgreSQL.Simple

import qualified Database.PostgreSQL.Queue as Q
import qualified Database.PostgreSQL.Schedule as S

-- | Run a sub-command.
run :: Command.Command -> IO ()
run cmd = do
  con <- connectPostgreSQL . pack $ Command.cmdDatabaseUrl cmd
  _ <- execute_ con "SET application_name='humming'"
  case cmd of
    Command.Create{..} -> do
      Q.create con
      when (not cmdNoScheduling) $ S.create con
    Command.Drop{} -> do
      Q.drop con
      S.drop con
    Command.Enqueue{..} -> do
      case parse json (L.pack cmdArguments) of
        Done _ arguments -> do
          let q = Q.Queue $ pack cmdQueueName
          i <- Q.enqueue con q (pack cmdMethod) arguments
          putStrLn $ "Enqueued job #" ++ show i ++ "."
        _ -> putStrLn "The argument ain't no valid JSON."
    Command.Count{..} -> do
      Q.runCount con (fmap pack cmdMQueueName) >>= putStrLn . show
    Command.Delete{..} -> do
      case (cmdMQueueName, cmdMJobId) of
        (Just _, Just _) ->
          putStrLn "Only at most one of --queue and --job can be given."
        (Just queueName, _) -> Q.runDeleteQueue con $ pack queueName
        (_, Just jobId) -> Q.runDeleteJob con jobId
        (_, _) -> Q.runDeleteAll con
    Command.Lock{..} -> do
      Q.runLock con (pack cmdQueueName) 10 >>= print
    Command.UnlockDeads{} -> do
      Q.unlockJobsOfDeadWorkers con
    Command.Listen{..} -> do
      Q.listenNotifications con cmdQueueName
    Command.Notify{..} -> do
      Q.sendNotification con cmdQueueName
    Command.Work{..} -> do
      w_ <- Q.defaultWorker
      let w = w_ { Q.workerQueue = Q.Queue (pack cmdQueueName) }
      if cmdWorkOnce
        then Q.work con w
        else Q.start con w
    Command.Schedule{} -> do
      S.schedule con
    Command.Plan{..} -> do
      case parse json (L.pack cmdArguments) of
        Done _ arguments -> do
          now <- getCurrentTime
          let at = now .+^ fromSeconds cmdSeconds
          S.plan con (T.pack cmdQueueName) (T.pack cmdMethod) arguments at
        _ -> putStrLn "The argument ain't no valid JSON."
  close con
