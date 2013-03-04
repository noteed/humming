{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 ()
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
    &= help "Push a job on a queue."
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
        con <- connectPostgreSQL $ pack connectionString
        let q = Q.Queue "yeah" Nothing
        Q.enqueue con q "go" ()
