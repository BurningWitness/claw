{-# LANGUAGE CPP
           , OverloadedStrings #-}

module Main
  ( main
  ) where

import           Example.Core
import           Example.Options

import           Data.Coerce
import           Prettyprinter
import           Prettyprinter.Render.Text
import           System.Console.Options
import           System.Console.Options.Failure
import           System.IO
import           System.Exit
import           System.OsString.Internal.Types

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import           System.Win32.WindowsString.Console
#else
import           System.Posix.Env.PosixString
#endif



main :: IO ()
main = do
  args <-
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
          (coerce :: IO [WindowsString] -> IO [OsString])
#else
          (coerce :: IO [PosixString] -> IO [OsString])
#endif
            getArgs

  example args



example :: [OsString] -> IO ()
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
