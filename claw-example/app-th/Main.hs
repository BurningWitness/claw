{-# LANGUAGE CPP
           , OverloadedStrings
           , TemplateHaskell #-}

module Main
  ( main
  ) where

import           Example.Core
import           Example.Options.TH

import           Data.Coerce
import qualified Data.Text.IO as Text
import           Language.Haskell.TH.Syntax
import           Prettyprinter
import           Prettyprinter.Render.Text
import           System.Console.Options.TH
import           System.Console.Options.TH.Failure
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



options :: Options Identity (State -> Either Breaker State)
options = $$(precompile optionsQ)

example :: [OsString] -> IO ()
example input = do
  case run (decode Permute options) defaultState input of
    Success (State mayBase mayOperation mayVerbosity) args ->
      execute mayVerbosity mayBase mayOperation args

    Break r ->
      case r of
        Benign benign ->
          Text.hPutStrLn stderr $
            case benign of
              Help    -> $(lift . renderStrict $ layoutPretty defaultLayoutOptions helpDoc)
              Version -> $(lift . renderStrict $ layoutCompact versionDoc)

        Error err      -> do
          renderIO stderr . layoutCompact $ "claw-example: " <> errorDoc err <> hardline
          exitWith (ExitFailure 1)

    Failure err -> do
      renderIO stderr $
        layoutCompact $ "claw-example: " <> failure options err <> hardline

      exitWith (ExitFailure 1)
