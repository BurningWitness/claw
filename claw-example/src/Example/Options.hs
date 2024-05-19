{-# LANGUAGE OverloadedStrings
           , QuasiQuotes #-}

module Example.Options
  ( State (..)
  , defaultState

  , Breaker (..)
  , Benign (..)
  , Error (..)

  , options

  , helpDoc
  , versionDoc
  , errorDoc
  ) where

import           Example.Options.State

import           Data.List.NonEmpty
import           Data.Text (Text)
import           Prettyprinter
import           Prettyprinter.Util
import           System.Console.Options
import           System.Console.Options.Help
import           System.OsString (osstr)
import           System.OsString.Internal.Types



options :: Options Identity (State -> Either Breaker State)
options =
     none
  .> help
  .> version
  .> base
  .> binary
  .> octal
  .> decimal
  .> hexadecimal
  .> operation
  .> quiet
  .> verbose



help
  , version
  , base
  , binary
  , octal
  , decimal
  , hexadecimal
  , operation
  , quiet
  , verbose
 :: Option Identity (Doc ann) (State -> Either Breaker State)
help        = Option (Short 'h' :| [Long [osstr|help|]]) (plain $ \_ -> Left (Benign Help))
version     = Option (Long [osstr|version|] :| [])       (plain $ \_ -> Left (Benign Version))

base        = Option (Short 'b' :| [Long [osstr|base|]]) (required "NUMBER" updateBase)
binary      = Option (Long [osstr|binary|]      :| []) (plain $ setBase 2 )
octal       = Option (Long [osstr|octal|]       :| []) (plain $ setBase 8 )
decimal     = Option (Long [osstr|decimal|]     :| []) (plain $ setBase 10)
hexadecimal = Option (Long [osstr|hexadecimal|] :| []) (plain $ setBase 16)

operation   = Option (Short 'o' :| [Long [osstr|operation|]]) (required "NAME" updateOperation)

quiet       = Option (Short 'q' :| [Long [osstr|quiet|]]) (plain setQuiet)
verbose     = Option (Short 'v' :| [Long [osstr|verbose|]]) (optional "LEVEL" updateVerbosity)



section :: [(Option m (Doc ann) f, [Text])] -> Doc ann
section docs =
  nest 2 $
    foldr (\a b -> hardline <> a <> b) emptyDoc $
      fmap (\(opt, doc) ->
               description 30 8 opt
                 (concatWith (\a b -> a <> hardline <> b) $ fmap reflow doc)
           )
        docs



helpDoc :: Doc ann
helpDoc =
     fillSep
       ( foldMap Prettyprinter.Util.words
           [ "Example program to showcase commandline options."
           , "Parses input integers at the given base,"
           , "applies the chosen operation and outputs a floating-point number."
           ]
       )
  <> hardline
  <> hardline
  <> reflow "Usage: claw-example [options] numbers"
  <> hardline
  <> hardline
  <> "General options"
  <> section
       [ (,) help    ["Show this help text."]
       , (,) version ["Print version information."]
       , (,) verbose ["Verbosity. Supports levels 0-2, default 1."]
       , (,) quiet   ["Do not output any intermediate logs."]
       ]
  <> hardline
  <> hardline
  <> "Bases"
  <> section
       [ (,) base        ["Set number base.", "Bases in 2-16 range (inclusive) are supported."]
       , (,) binary      ["Set number base to 2."]
       , (,) octal       ["Set number base to 8."]
       , (,) decimal     ["Set number base to 10."]
       , (,) hexadecimal ["Set number base to 16."]
       ]

  <> hardline
  <> hardline
  <> "Operations"
  <> section
       [ (,) operation ["Choose an operation, currently any of: add, multiply, average, median"]
       ]
  <> hardline



versionDoc :: Doc ann
versionDoc =
     "claw-example 0.1.0.0"
  <> hardline



errorDoc :: Error -> Doc ann
errorDoc e =
  let kind = case coercionToPlatformTypes of
               Left _  -> "16LE"
               Right _ -> "8"

      encodingError = " argument is a malformed UTF-" <> kind <> " stream"

  in case e of
       InvalidBaseEncoding      -> "Base " <> encodingError
       NotABase str             -> "'" <> pretty str <> "' is not a valid base"
       BaseOutOfBounds i        -> "Base '" <> pretty i <> "' is not in the 2-16 range"
       InvalidOperationEncoding -> "Operation " <> encodingError
       UnknownOperation str     -> "'" <> pretty str <> "' is not a valid operation"
       InvalidVerbosityEncoding -> "Verbosity " <> encodingError
       UnknownVerbosity str     -> "'" <> pretty str <> "' is not a valid verbosity"
