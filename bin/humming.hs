{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Aeson (json)
import Data.Attoparsec.Lazy (parse, Result(..))
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as L
import Database.PostgreSQL.Simple
import System.Console.CmdArgs.Implicit
import System.Environment (getEnvironment)

import qualified Database.PostgreSQL.Queue as Q

main :: IO ()
main = (runCmd =<<) $ cmdArgs $
  modes
    [ cmdCreate
    , cmdDrop
    , cmdEnqueue
    ]
  &= summary versionString
  &= program "humming"

-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "humming - Copyright (c) 2013 Vo Minh Thu."
  -- TODO add the version.

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
  deriving (Data, Typeable)

-- | Create a 'Create' command.
cmdCreate :: Cmd
cmdCreate = Create
    &= help "Create the queue_classic table."
    &= explicit
    &= name "create"

-- | Create a 'Drop' command.
cmdDrop :: Cmd
cmdDrop = Drop
    &= help "Drop the queue_classic table."
    &= explicit
    &= name "drop"

-- | Create an 'Enqueue' command.
cmdEnqueue :: Cmd
cmdEnqueue = Enqueue
  { cmdQueueName = def
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
