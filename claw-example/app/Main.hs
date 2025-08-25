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
import           System.IO
import           System.Exit
import           System.Process.Environment.OsString (getArgs)



main :: IO ()
main = do
  raws <- getArgs
  s <- newMutableState
  out <- parse options s raws
  case out of
    Success args -> do
      State mayBase mayOperation mayVerbosity <- extractState s
      execute mayVerbosity mayBase mayOperation args

    Broken r ->
      case r of
        Benign benign ->
          renderIO stderr $
            case benign of
              Help    -> layoutPretty defaultLayoutOptions helpDoc
              Version -> layoutCompact versionDoc

        Error err      -> do
          hPutStrLn stderr $ showString "claw-example: " $ err ""
          exitWith (ExitFailure 1)

    Failure err -> do
      renderIO stderr $
        layoutCompact $ "claw-example: " <> failure options err <> hardline

      exitWith (ExitFailure 1)
