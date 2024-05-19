{-# LANGUAGE GADTs
           , OverloadedStrings #-}

module System.Console.Options.Help.Internal
  ( malformed
  , name
  , option
  , description
  ) where

import           Data.Console.Option
import           Data.List.NonEmpty (NonEmpty (..))
import           Prettyprinter
import           Prettyprinter.Internal.Type
import qualified System.OsString as Os
import           System.OsString.Internal.Types



malformed :: Doc ann
malformed =
  let kind = case coercionToPlatformTypes of
               Left _  -> "16LE"
               Right _ -> Char '8'

  in "<malformed UTF-" <> kind <> " stream>"



-- | Option name.
--
--   >>> putDocW 80 $ name (Short 'h')
--   -h
--   >>> putDocW 80 $ name (Long [osstr|'long-foo'|])
--   --long-foo
name :: Name -> Doc ann
name (Short c) =  "-" <> pretty c
name (Long ls) = "--" <> maybe malformed pretty (Os.decodeUtf ls)



-- | Option together with the argument name, if applicable.
--
--   >>> putDocW 80 . option $ Option (Short 'f' :| [Long "foo"]) (plain id)
--   -f, --foo
--   >>> putDocW 80 . option $ Option (Long "bar" :| [Long "baz"]) (required "FILE" id)
--   --bar, --baz=FILE
option :: Option m (Doc ann) f -> Doc ann
option (Option (n :| ns) f) =
  name n <> foldr (\a b -> softline' <> ", " <> name a <> b) (flavor f) ns
  where
    flavor (Plain        _) = emptyDoc
    flavor (Optional doc _) = brackets (equals <> doc)
    flavor (Required doc _) = equals <> doc



-- | Help description for a given option.
--
--   The help text is laid out at base indentation. Additionally the first line
--   of help text can shift based on the increment, in the case that the list
--   of option names is longer than base indentation.
--
--   >>> let opt = Option (Short 'f' :| [Long "foo", Long "tediously-long-bar", Long "baz"]) (Flavor (Required "FIL    E") id)
--   >>> let help = reflow "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor..."
--   >>> putDocW 80 $ description 32 8 opt help
--   -f, --foo, --tediously-long-bar, --baz=FILE     Lorem ipsum dolor sit amet,
--                                   consectetur adipiscing elit, sed do eiusmod
--                                   tempor...
description
  :: Int                  -- ^ Base help text indentation
  -> Int                  -- ^ Indentation increment
  -> Option m (Doc ann) f
  -> Doc ann              -- ^ Help text
  -> Doc ann
description offset fracs opt@(Option (first :| _) _) doc =
  width
    ( case first of
        Short _ ->           nest 2 (option opt)
        Long _  -> "    " <> nest 2 (option opt)
    )
    ( \i ->
       nest offset $
         let spaces
               | i + 1 < offset = offset - i
               | otherwise      = let r = (i - offset) `rem` fracs

                                  in if r + 2 > fracs
                                       then fracs + (fracs - r)
                                       else fracs - r

         in Union (pretty $ replicate spaces ' ') hardline <> doc
    )
