{-# LANGUAGE TemplateHaskellQuotes #-}

{- | Runtime-evaluated command-line option parsing.

     == Implementation

     Description of the argument syntax can be found at
     [GNU Program Argument Syntax Conventions](https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html)
     page, which extends the short-option-only
     [POSIX Utility Conventions](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html).
 -}

module System.Console.Options
  ( Name (..)

    -- * Construct
  , Option (..)

    -- | === Flavor
  , Flavor
  , plain
  , optional
  , required

    -- ** Combine
  , Options
  , none
  , (.>)

    -- ** Precompile
    -- | Refer to "System.Console.Options.TH" on how to construct 'OptionsQ'.
  , precompile

    -- * Analyze
  , suggestions

    -- * Parse
    -- | === Decoder
  , Order (..)
  , Decoder
  , decode

    -- | === Runner
  , Failure (..)
  , Result (..)
  , run

    -- | Refer to "Codec.Console.Options" for both the unwrapping of the 'Decoder'
    --   and other runner functions.

    -- * Re-exports
  , Identity (..)
  ) where

import           Codec.Console.Options.Internal
import           Data.Console.Option.Internal
import           Data.Console.Options.Lazy.Internal
import           Data.Console.Options.Strict.Internal (OptionsQ, sequenceCode, toLazy)

import           Data.Functor.Identity
import           Language.Haskell.TH.Syntax hiding (Name)
import           System.OsString (OsString)



-- | Option takes no arguments.
plain :: f -> Flavor Identity help f
plain = Plain . Identity

-- | Option takes an optional argument.
optional
  :: help                   -- ^ Help description for the argument,
                            --   i.e. @ARG@ in @-o, --option[=ARG]@.
  -> (Maybe OsString -> f)
  -> Flavor Identity help f
optional h = Optional h . Identity

-- | Option requires an argument.
required
  :: help                   -- ^ Help description for the argument,
                            --   i.e. @ARG@ in @-o, --option=ARG@.
  -> (OsString -> f)
  -> Flavor Identity help f
required h = Required h . Identity



infixl 5 .>
-- | Insert an option into the dictionary.
--
--   If the option has an empty list of names, nothing is added to the dictionary.
--   Empty long names are similarly treated as no-ops.
--   When inserting duplicate names new values take precedence.
(.>) :: Options Identity f -> Option Identity help f -> Options Identity f
(.>) = flip insert

-- | Empty option dictionary.
none :: Options Identity f
none = empty



-- | Precompile the option dictionary.
precompile :: OptionsQ f -> Code Q (Options Identity f)
precompile q =
  joinCode $
    pure [|| toLazy $$(sequenceCode q) ||]



{-# INLINE decode #-}
-- | Construct a decoder.
decode :: Order -> Options Identity f -> Decoder f
decode order opts =
  Decoder $
    step (flip Data.Console.Options.Lazy.Internal.lookup opts) order
