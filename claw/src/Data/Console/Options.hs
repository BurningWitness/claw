{- | t'Options' is a spine-strict tree of command-line options.

     == Laziness

     Evaluating the root of the tree (i.e. @(_ :: t'Options' m f)@) to
     weak head normal form evaluates the entire spine of the tree to normal form.

 -}

module Data.Console.Options
  ( -- * Itself
    OptionsQ
  , Options

    -- * Construct
  , empty

    -- * Insert
  , Name (..)
  , Flavor (..)
  , Option (..)
  , insert
  , insertQ

    -- * Precompile
  , sequenceCode

    -- * Analyze
  , suggestions
  ) where

import           Data.Console.Option.Internal
import           Data.Console.Options.Internal
