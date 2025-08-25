{-# LANGUAGE DuplicateRecordFields
           , NoFieldSelectors
           , OverloadedRecordDot
           , RecordWildCards #-}

module Example.Options.State
  ( MutableState (..)
  , newMutableState

  , State (..)
  , extractState

  , Breaker (..)
  , Benign (..)

  , getHelp
  , getVersion

  , setBase
  , updateBase

  , Operation (..)
  , updateOperation

  , Verbosity (..)
  , setQuiet
  , updateVerbosity
  ) where

import           Example.Core

import           Data.IORef
import           Text.Read
import           System.OsPath (OsString, decodeFS)



data MutableState =
       MutableState
         { base      :: !(IORef (Maybe Base))
         , operation :: !(IORef (Maybe Operation))
         , verbosity :: !(IORef (Maybe Verbosity))
         }

newMutableState :: IO MutableState
newMutableState = do
  base      <- newIORef Nothing
  operation <- newIORef Nothing
  verbosity <- newIORef Nothing
  pure $! MutableState {..}



data State =
       State
         { base      :: Maybe Base
         , operation :: Maybe Operation
         , verbosity :: Maybe Verbosity
         }

extractState :: MutableState -> IO State
extractState s = do
  base      <- readIORef s.base
  operation <- readIORef s.operation
  verbosity <- readIORef s.verbosity
  pure $! State {..}




data Breaker = Benign Benign
             | Error ShowS



data Benign = Help
            | Version

getHelp, getVersion :: MutableState -> IO (Either Breaker ())
getHelp _    = pure . Left $ Benign Help
getVersion _ = pure . Left $ Benign Version



setBase :: Int -> MutableState -> IO (Either Breaker ())
setBase i s = do
  writeIORef s.base $! Just $! Base i
  pure $ Right ()

updateBase :: OsString -> MutableState -> IO (Either Breaker ())
updateBase t s = do
  str <- decodeFS t
  case readMaybe str of
    Nothing ->
      let err = showChar '\'' . (<> str) . showString "is not a valid base"
      in pure . Left $! Error err

    Just i
      | i < 2 || i > 16 ->
          let err = showString "Base '" . shows i . showString "' is not in the 2-16 range"
          in pure . Left $! Error err

      | otherwise       -> setBase i s



updateOperation :: OsString -> MutableState -> IO (Either Breaker ())
updateOperation t s = do
  str <- decodeFS t

  let set op = do
        writeIORef s.operation $! Just op
        pure $ Right ()

      err = showChar '\'' . (str <>) . showString "' is not a valid operation"

  case str of
    "sum"     -> set Sum
    "product" -> set Product
    "average" -> set Average
    "median"  -> set Median
    _         -> pure . Left $ Error err



setQuiet :: MutableState -> IO (Either Breaker ())
setQuiet s = do
  writeIORef s.verbosity $! Just Quiet
  pure $ Right ()

updateVerbosity :: Maybe OsString -> MutableState -> IO (Either Breaker ())
updateVerbosity mayx s = do
  let set verb = do
        writeIORef s.verbosity $! Just verb
        pure $ Right ()

  case mayx of
    Nothing  -> set Verbose1
    Just x   -> do
      str <- decodeFS x

      let err = showChar '\'' . (str <>) . showString "' is not a valid verbosity"
      case str of
        "0" -> set Quiet
        "1" -> set Verbose1
        "2" -> set Verbose2
        _   -> pure . Left $ Error err
