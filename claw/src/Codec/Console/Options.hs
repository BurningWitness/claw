{- | Decoder definition and several predefined runners.
 -}

module Codec.Console.Options
  ( -- | === Decoder
    Decoder (..)
  , Step (..)

    -- | === Runner
  , Name (..)
  , Failure (..)
  , Result (..)
  , run
  , runM
  , runInOrder
  ) where

import           Codec.Console.Options.Internal
import           Data.Console.Option.Internal
