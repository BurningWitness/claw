{-# LANGUAGE DuplicateRecordFields
           , OverloadedStrings
           , TemplateHaskell #-}

module Example.Options
  ( MutableState
  , newMutableState

  , State (..)
  , extractState

  , Breaker (..)
  , Benign (..)

  , options

  , helpDoc
  , versionDoc
  ) where

import           Example.Options.State

import           Data.Console.Options as Options
import           Data.Console.Options.Help
import           Data.Functor.Identity
import           Data.Text (Text)
import           Prettyprinter
import           Prettyprinter.Util
import           System.OsString (OsString)



options :: Options Identity (MutableState -> IO (Either Breaker ()))
options =
    Options.insert help        getHelp
  . Options.insert version     getVersion
  . Options.insert base        updateBase
  . Options.insert binary      (setBase 2)
  . Options.insert octal       (setBase 8)
  . Options.insert decimal     (setBase 10)
  . Options.insert hexadecimal (setBase 16)
  . Options.insert operation   updateOperation
  . Options.insert quiet       setQuiet
  . Options.insert verbose     updateVerbosity
  $ Options.empty



help, version :: Option help f f
help    = Option [Short 'h', Long "help"] Plain
version = Option [Long "version"]         Plain

base :: Option (Doc ann) f (OsString -> f)
base = Option [Short 'b', Long "base"] (Required "NUMBER")

binary, octal, decimal, hexadecimal :: Option help f f
binary      = Option [Long "binary"]      Plain
octal       = Option [Long "octal"]       Plain
decimal     = Option [Long "decimal"]     Plain
hexadecimal = Option [Long "hexadecimal"] Plain

operation :: Option (Doc ann) f (OsString -> f)
operation = Option [Short 'o', Long "operation"] (Required "NAME")

quiet :: Option (Doc ann) f f
quiet = Option [Short 'q', Long "quiet"] Plain

verbose :: Option (Doc ann) f (Maybe OsString -> f)
verbose = Option [Short 'v', Long "verbose"] (Optional "LEVEL")



section :: Doc ann -> [Doc ann] -> Doc ann
section sname entries =
     hardline
  <> hardline
  <> sname
  <> nest 2 (foldMap (hardline <>) entries)

entry :: Option (Doc ann) f arg -> [Text] -> Doc ann
entry opt inside =
  description 30 8 opt $
    concatWith (\a b -> a <> hardline <> b) $ fmap reflow inside



helpDoc :: Doc ann
helpDoc =
     reflow
       "Example program to showcase commandline options. \
       \Parses input integers at the given base, \
       \applies the chosen operation and outputs a floating-point number."

  <> hardline
  <> hardline
  <> reflow "Usage: claw-example [options] numbers"

  <> section "General options"
       [ entry help    ["Show this help text."]
       , entry version ["Print version information."]
       , entry verbose ["Verbosity. Supports levels 0-2, default 1."]
       , entry quiet   ["Do not output any intermediate logs."]
       ]

  <> section "Bases"
       [ entry base        ["Set number base.", "Bases in 2-16 range (inclusive) are supported."]
       , entry binary      ["Set number base to 2."]
       , entry octal       ["Set number base to 8."]
       , entry decimal     ["Set number base to 10."]
       , entry hexadecimal ["Set number base to 16."]
       ]

  <> section "Operations"
       [ entry operation ["Choose an operation, currently any of: add, multiply, average, median"]
       ]

  <> hardline



versionDoc :: Doc ann
versionDoc =
     "claw-example 0.2.0.0"
  <> hardline
