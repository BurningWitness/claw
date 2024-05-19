{-# LANGUAGE NoFieldSelectors #-}

module Example.Options.State
  ( State (..)
  , defaultState

  , Breaker (..)
  , Benign (..)
  , Error (..)

  , setBase
  , updateBase

  , Operation (..)
  , updateOperation

  , Verbosity (..)
  , setQuiet
  , updateVerbosity
  ) where

import           Example.Core

import           System.OsString (OsString)
import qualified System.OsString as Os
import           Text.Read



data State =
       State
         { base      :: Maybe Base
         , operation :: Maybe Operation
         , verbosity :: Maybe Verbosity
         }

defaultState :: State
defaultState =
  State
    { base      = Nothing
    , operation = Nothing
    , verbosity = Nothing
    }




data Breaker = Benign Benign
             | Error Error

data Benign = Help
            | Version

data Error = InvalidBaseEncoding
           | NotABase String
           | BaseOutOfBounds Int
           | InvalidOperationEncoding
           | UnknownOperation String
           | InvalidVerbosityEncoding
           | UnknownVerbosity String



setBase :: Int -> State -> Either Breaker State
setBase i s = Right $! s { base = Just (Base i) }

updateBase :: OsString -> State -> Either Breaker State
updateBase xx s =
  case Os.decodeUtf xx of
    Nothing -> Left $ Error InvalidBaseEncoding
    Just t  ->
      case readMaybe t of
        Nothing -> Left . Error $ NotABase t
        Just i
          | i < 2 || i > 16 -> Left . Error $ BaseOutOfBounds i
          | otherwise       -> Right $! s { base = Just (Base i) }



updateOperation :: OsString -> State -> Either Breaker State
updateOperation xx s =
  let set o = Right $! s { operation = Just o }
  in case Os.decodeUtf xx of
       Nothing -> Left $ Error InvalidOperationEncoding
       Just x  ->
         case x of
           "sum"     -> set Sum
           "product" -> set Product
           "average" -> set Average
           "median"  -> set Median
           _         -> Left . Error $ UnknownOperation x



setQuiet :: State -> Either Breaker State
setQuiet s = Right $! s { verbosity = Just Quiet }

updateVerbosity :: Maybe OsString -> State -> Either Breaker State
updateVerbosity mayx s =
  let set v = Right $! s { verbosity = Just v }
  in case mayx of
       Nothing  -> set Verbose1
       Just xx  ->
         case Os.decodeUtf xx of
           Nothing  -> Left $ Error InvalidVerbosityEncoding
           Just x   ->
             case x of
               "0" -> set Quiet
               "1" -> set Verbose1
               "2" -> set Verbose2
               _   -> Left . Error $ UnknownVerbosity x
