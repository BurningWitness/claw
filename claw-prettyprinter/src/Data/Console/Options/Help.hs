{-# LANGUAGE GADTs
           , OverloadedStrings #-}

{- | Pretty-printing functions for composing the help document.
 -}

module Data.Console.Options.Help
  ( name
  , option
  , description
  ) where

import           Data.Console.Options
import qualified Data.Text.Short as ST
import           Prettyprinter
import           Prettyprinter.Internal.Type



-- | Option name.
--
--   >>> name (Short 'h')
--   -h
--   >>> name (Long "long-foo")
--   --long-foo
name :: Name -> Doc ann
name (Short c) =  "-" <> pretty c
name (Long ls) = "--" <> pretty (ST.toText ls)



-- | Option together with the argument name, if applicable.
--
--   >>> option $ Option [Short 'f', Long "foo"] Plain
--   -f, --foo
--   >>> option $ Option [Long "bar", Long "baz"] (Required "FILE")
--   --bar, --baz=FILE
option :: Option (Doc ann) f arg -> Doc ann
option (Option ns f) =
  case ns of
    []    -> emptyDoc
    n:ns' -> name n <> foldr (\a b -> softline' <> ", " <> name a <> b) (flavor f) ns'
  where
    flavor :: Flavor (Doc ann) f arg -> Doc ann
    flavor  Plain         = emptyDoc
    flavor (Optional doc) = brackets (equals <> doc)
    flavor (Required doc) = equals <> doc



-- | Help description for a given option.
--
--   The help text is laid out at base indentation. Additionally the first line
--   of help text can shift based on the increment, in the case that the list
--   of option names is longer than base indentation.
--
--   >>> let help = reflow "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor..."
--   >>> let opt = Option [Long "foo"] Plain
--   >>> putDocW 80 $ description 32 8 opt help
--       --foo                       Lorem ipsum dolor sit amet, consectetur
--                                   adipiscing elit, sed do eiusmod tempor...
--   >>> let opt = Option [Short 'f', Long "foo", Long "tediously-long-bar", Long "baz"] (Required "FILE")
--   >>> putDocW 80 $ description 32 8 opt help
--   -f, --foo, --tediously-long-bar, --baz=FILE     Lorem ipsum dolor sit amet,
--                                   consectetur adipiscing elit, sed do eiusmod
--                                   tempor...
description
  :: Int                    -- ^ Base help text indentation
  -> Int                    -- ^ Indentation increment
  -> Option (Doc ann) f arg
  -> Doc ann                -- ^ Help text
  -> Doc ann
description offset fracs opt@(Option names _) doc =
  width
    ( let base = nest 2 (option opt)
      in case names of
           Long _ : _ -> "    " <> base
           _          ->           base
    )
    ( \i ->
       nest offset $
         let spaces
               | i + 1 < offset = offset - i
               | otherwise      = let r = (i - offset) `rem` fracs

                                  in if r + 2 > fracs
                                       then fracs + (fracs - r)
                                       else fracs - r

         in Union (Text spaces . ST.toText $ ST.replicate spaces " ") hardline <> doc
    )
