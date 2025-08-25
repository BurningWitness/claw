{- | t'Failure' pretty printing.
 -}

{-# LANGUAGE OverloadedStrings #-}

module System.Console.Options.Failure
  ( failure

    -- | === Constituents
  , unrecognized
  , unsaturated
  , oversaturated
  ) where

import           Data.Console.Options.Help

import           Data.ByteString.Short (ShortByteString)
import           Data.Console.Options
import           Data.Functor.Identity
import           Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as ST
import           Prettyprinter
import           Prettyprinter.Internal.Type
import           System.Console.Options



-- | Failure description, with 'suggestions' if applicable.
--
--   Examples for each specific failure case are given below.
failure :: Options Identity f -> Failure -> Doc ann
failure opts f =
  case f of
    NonPortable _ arg    -> nonPortable arg
    Unrecognized _ arg n -> unrecognized opts arg n
    Unsaturated n        -> unsaturated n
    Oversaturated ls _   -> oversaturated ls



-- | 'NonPortable' failure case.
--
--   >>> nonPortable "--non\xED\xB3\xBF\&ascii"
--   option 'nonascii�' is uninterpretable
nonPortable
  :: ShortByteString -- ^ Argument converted to WTF-8.
  -> Doc ann
nonPortable arg =
  let sanitized = ST.pack . ST.unpack $ ST.fromShortByteStringUnsafe arg
  in "option '" <> pretty (ST.toText sanitized) <> "'"



-- | 'Unrecognized' failure case, with 'suggestions' if applicable.
--
--   >>> let opt = Option [Short 'i', Short 'a', Short 'b', Long "abode"] Plain
--   >>> let opts = insert opt id none
--   >>> unrecognized opts "--abid" (Long "abid")
--   invalid option '--abid'; perhaps you meant any of '-a', '-b', '-i' or '--abode'?
unrecognized
  :: Options Identity f
  -> ShortByteString    -- ^ Argument converted to WTF-8.
  -> Name
  -> Doc ann
unrecognized opts arg n =
  "invalid option '" <> name n <> squote
    <> case suggestions opts arg n of
         []     -> emptyDoc
         ss     ->
           "; perhaps you meant "
             <> ( case ss of
                    [x]    -> squotes (name x)
                    x:y:zs -> "any of " <> orFold (squotes (name x)) (squotes (name y))
                                             (squotes . name <$> zs)
                )
             <> Char '?'

-- | @_, _, _, _ or _@.
orFold :: Doc ann -> Doc ann -> [Doc ann] -> Doc ann
orFold x y zs =
  case zs of
    []   -> x <> " or " <> y
    z:as -> x <> ", " <> orFold y z as



-- | 'Unsaturated' failure case.
--
--   >>> unsaturated (Long "foo")
--   option '--foo' requires an argument
unsaturated :: Name -> Doc ann
unsaturated n = "option '" <> name n <> "' requires an argument"

-- | 'Oversaturated' failure case.
--
--   >>> oversaturated "foo"
--   option '--foo' doesn't allow an argument
--   >>> oversaturated ""
--   end of options delimiter '--' doesn't allow an argument
oversaturated
  :: ShortText -- ^ Long option name, may be empty (consider @--=ARG@).
  -> Doc ann
oversaturated ls =
     ( case ls of
        "" -> "end of options delimiter '--"
        _  -> "option '--" <> pretty (ST.toText ls)
     )
  <> "' doesn't allow an argument"
