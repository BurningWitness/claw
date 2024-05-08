{- | Precompiled command-line option parsing.

     The interface is near-identical to the "System.Console.Options" one:
     the only differences are that each option function needs to be put into
     typed expression quotes (@[|| _ ||]@), and the option dictionary needs to
     be bound using a typed splice (@$$('precompile' _)@).
 -}

module System.Console.Options.TH
  ( Name (..)

    -- * Construct
  , Option (..)

    -- | === Flavor
  , Flavor
  , plain
  , optional
  , required

    -- ** Combine
  , OptionsQ
  , none
  , (?>)
  , (.>)

    -- ** Precompile
  , Options
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
  , Q
  , Code
  ) where

import           Codec.Console.Options.Internal
import           Data.Console.Option.Internal
import           Data.Console.Options.Strict.Internal

import           Data.Functor.Identity
import           Language.Haskell.TH.Syntax hiding (Name)



-- | Option takes no arguments.
plain :: Code Q f -> Flavor (Code Q) help f
plain = Plain

-- | Option takes an optional argument.
optional
  :: help                       -- ^ Help description for the argument,
                                --   i.e. @ARG@ in @-o, --option[=ARG]@.
  -> Code Q (Maybe String -> f)
  -> Flavor (Code Q) help f
optional = Optional

-- | Option requires an argument.
required
  :: help                   -- ^ Help description for the argument,
                            --   i.e. @ARG@ in @-o, --option=ARG@.
  -> Code Q (String -> f)
  -> Flavor (Code Q) help f
required = Required



add :: Verbosity -> OptionsQ f -> Option (Code Q) help f -> OptionsQ f
add verbosity opts o = insert verbosity o =<< opts

infixl 5 ?>
-- | Insert an option into the dictionary, emitting warnings if any
--   issues are detected.
--
--   Behaves the same as '.>' otherwise.
(?>) :: OptionsQ f -> Option (Code Q) help f -> OptionsQ f
(?>) = add Warn

infixl 5 .>
-- | Insert an option into the dictionary, silently.
--
--   If the option has an empty list of names, nothing is added to the dictionary.
--   Empty long names are similarly treated as no-ops.
--   When inserting duplicate names new values take precedence.
(.>) :: OptionsQ f -> Option (Code Q) help f -> OptionsQ f
(.>) = add Silent

-- | Empty option dictionary.
none :: OptionsQ f
none = pure empty



-- | Precompile the option dictionary.
precompile :: OptionsQ f -> Code Q (Options Identity f)
precompile = sequenceCode



{-# INLINE decode #-}
-- | Construct a decoder.
decode :: Order -> Options Identity f -> Decoder f
decode order opts =
  Decoder $
    step (flip Data.Console.Options.Strict.Internal.lookup opts) order
