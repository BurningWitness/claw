{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Example.Core
import           Example.Options

import           Prettyprinter
import           Prettyprinter.Render.Text
import           System.Console.Options
import           System.Console.Options.Failure
import           System.Environment
import           System.IO
import           System.Exit



main :: IO ()
main = do
  args <- getArgs
  example args



example :: [String] -> IO ()
example input = do
  case run (decode Permute options) defaultState input of
    Success (State mayBase mayOperation mayVerbosity) args ->
      execute mayVerbosity mayBase mayOperation args

    Break r ->
      case r of
        Benign benign ->
          renderIO stderr $
            case benign of
              Help    -> layoutPretty defaultLayoutOptions helpDoc
              Version -> layoutCompact versionDoc

        Error err      -> do
          renderIO stderr . layoutCompact $ "claw-example: " <> errorDoc err <> hardline
          exitWith (ExitFailure 1)

    Failure err -> do
      renderIO stderr $
        layoutCompact $ "claw-example: " <> failure options err <> hardline

      exitWith (ExitFailure 1)
