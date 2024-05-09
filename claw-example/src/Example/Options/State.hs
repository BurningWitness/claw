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

data Error = NotABase String
           | BaseOutOfBounds Int
           | UnknownOperation String
           | UnknownVerbosity String



setBase :: Int -> State -> Either Breaker State
setBase i s = Right $! s { base = Just (Base i) }

updateBase :: String -> State -> Either Breaker State
updateBase t s =
  case readMaybe t of
    Nothing -> Left . Error $ NotABase t
    Just i
      | i < 2 || i > 16 -> Left . Error $ BaseOutOfBounds i
      | otherwise       -> Right $! s { base = Just (Base i) }



updateOperation :: String -> State -> Either Breaker State
updateOperation x s =
  let set o = Right $! s { operation = Just o }
  in case x of
       "sum"     -> set Sum
       "product" -> set Product
       "average" -> set Average
       "median"  -> set Median
       _         -> Left . Error $ UnknownOperation x



setQuiet :: State -> Either Breaker State
setQuiet s = Right $! s { verbosity = Just Quiet }

updateVerbosity :: Maybe String -> State -> Either Breaker State
updateVerbosity mayx s =
  let set v = Right $! s { verbosity = Just v }
  in case mayx of
       Nothing  -> set Verbose1
       Just "0" -> set Quiet
       Just "1" -> set Verbose1
       Just "2" -> set Verbose2
       Just x   -> Left . Error $ UnknownVerbosity x
