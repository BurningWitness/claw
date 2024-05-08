{-# LANGUAGE OverloadedStrings #-}

module System.Console.Options.Failure.Internal
  ( mkUnrecognized
  , unsaturated
  , oversaturated
  ) where

import           System.Console.Options.Help.Internal

import           Data.Text (Text)
import           Prettyprinter
import           Prettyprinter.Internal.Type
import           System.Console.Options (Name)



{-# INLINE mkUnrecognized #-}
mkUnrecognized :: (String -> Name -> [Name]) -> String -> Name -> Doc ann
mkUnrecognized suggest arg n =
  "invalid option '" <> name n <> squote
    <> case suggest arg n of
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



-- | 'Codec.Console.Options.Unsaturated' failure case.
--
--   >>> unsaturated (Long 'foo')
--   option '--foo' requires an argument
unsaturated :: Name -> Doc ann
unsaturated n = "option '" <> name n <> "' requires an argument"

-- | 'Codec.Console.Options.Oversaturated' failure case.
--
--   >>> oversaturated "foo"
--   option '--foo' doesn't allow an argument
--   >>> oversaturated ""
--   end of options delimiter '--' doesn't allow an argument
oversaturated
  :: Text    -- ^ Long option name, may be empty (consider @--=ARG@).
  -> Doc ann
oversaturated ls =
     ( case ls of
        "" -> "end of options delimiter '--"
        _  -> "option '--" <> pretty ls
     )
  <> "' doesn't allow an argument"
