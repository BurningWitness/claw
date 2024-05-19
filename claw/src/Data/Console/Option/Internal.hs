{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Console.Option.Internal
  ( Name (..)

  , Flavor (..)
  , Option (..)

  , Pair (..)
  , demote
  , demoteQ
  ) where

import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty)
import           Language.Haskell.TH.Syntax hiding (Name)
import           System.OsString



-- | Option name.
data Name = Short !Char    -- ^ As in @-s@.
          | Long !OsString -- ^ As in @--long-option@.



-- | Option behavior coupled with a function the option invokes.
data Flavor m help f = Plain         (m f)
                     | Optional help (m (Maybe OsString -> f))
                     | Required help (m (OsString -> f))



data Pair f = Zero  f
            | Maybe (Maybe OsString -> f)
            | One   (OsString -> f)



{-# INLINE demote #-}
demote :: Flavor Identity help f -> Pair f
demote (Plain      (Identity f)) = Zero  f
demote (Optional _ (Identity f)) = Maybe f
demote (Required _ (Identity f)) = One   f

{-# INLINE demoteQ #-}
demoteQ :: Flavor (Code Q) help f -> Code Q (Pair f)
demoteQ (Plain      f) = [|| Zero  $$(f) ||]
demoteQ (Optional _ f) = [|| Maybe $$(f) ||]
demoteQ (Required _ f) = [|| One   $$(f) ||]



-- | Single option.
data Option m help f =
       Option
         {-# UNPACK #-} !(NonEmpty Name) -- ^ Names which can be used to invoke the option.
         {-# UNPACK #-} !(Flavor m help f)
