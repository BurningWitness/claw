{- | Safe haven for the 'Flavor' type since it's needed for downstream printing, but
     wouldn't be convenient to have in the core user-facing modules.
 -}

module Data.Console.Option
  ( Name (..)
  , Option (..)
  , Flavor (..)
  ) where

import           Data.Console.Option.Internal
