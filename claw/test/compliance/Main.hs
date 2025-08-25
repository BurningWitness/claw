{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Test.Codec
import           Test.System

import           Data.Functor.Identity
import           Test.Hspec



main :: IO ()
main =
  hspec $ do
    describe "Codec"
      codec

    describe "System" $ do
      describe "Identity" $
        system options

      describe "Q" $
        system $$optionsQ
