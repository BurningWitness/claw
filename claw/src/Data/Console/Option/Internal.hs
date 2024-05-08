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
import           Data.Text (Text)
import           Language.Haskell.TH.Syntax hiding (Name)



-- | Option name.
data Name = Short !Char -- ^ As in @-s@.
          | Long !Text  -- ^ As in @--long-option@.



-- | Option behavior coupled with a function the option invokes.
data Flavor m help f = Plain         (m f)
                     | Optional help (m (Maybe String -> f))
                     | Required help (m (String -> f))



data Pair f = Zero  f
            | Maybe (Maybe String -> f)
            | One   (String -> f)



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
